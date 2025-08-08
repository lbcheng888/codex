use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComparePolicy {
    #[serde(default)]
    pub ignore_fields: Vec<String>,
    #[serde(default)]
    pub only_fields: Vec<String>,
    #[serde(default)]
    pub case_insensitive: bool,
    #[serde(default)]
    pub trim_space: bool,
    #[serde(default)]
    pub sort_arrays: bool,
    #[serde(default)]
    pub numeric_eps: Option<f64>,
    #[serde(default)]
    pub time_normalize: bool,
}

impl Default for ComparePolicy {
    fn default() -> Self {
        Self {
            ignore_fields: vec![],
            only_fields: vec![],
            case_insensitive: false,
            trim_space: true,
            sort_arrays: true,
            numeric_eps: Some(1e-9),
            time_normalize: false,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompareVerdict {
    pub equal: bool,
    #[serde(default)]
    pub diffs: Vec<String>,
}

pub fn compare(old: &Value, new: &Value, policy: &ComparePolicy) -> CompareVerdict {
    let mut diffs = Vec::new();
    let mut a = normalize(old.clone(), policy);
    let mut b = normalize(new.clone(), policy);

    if !policy.only_fields.is_empty() {
        a = project_fields(&a, &policy.only_fields);
        b = project_fields(&b, &policy.only_fields);
    }
    if !policy.ignore_fields.is_empty() {
        a = remove_fields(&a, &policy.ignore_fields);
        b = remove_fields(&b, &policy.ignore_fields);
    }

    if a == b {
        return CompareVerdict { equal: true, diffs };
    }

    // Best-effort structural diff (simple path-wise)
    collect_diffs("$", &a, &b, &mut diffs, policy);
    CompareVerdict { equal: diffs.is_empty(), diffs }
}

fn normalize(v: Value, policy: &ComparePolicy) -> Value {
    match v {
        Value::String(s) => {
            let mut s2 = s;
            if policy.trim_space { s2 = s2.trim().to_string(); }
            if policy.case_insensitive { s2 = s2.to_lowercase(); }
            Value::String(s2)
        }
        Value::Number(n) => Value::Number(n),
        Value::Array(arr) => {
            let mut arr2: Vec<Value> = arr.into_iter().map(|x| normalize(x, policy)).collect();
            if policy.sort_arrays {
                arr2.sort_by(|a, b| serde_json::to_string(a).unwrap_or_default().cmp(&serde_json::to_string(b).unwrap_or_default()));
            }
            Value::Array(arr2)
        }
        Value::Object(map) => {
            let mut out = serde_json::Map::new();
            for (k, v) in map.into_iter() {
                out.insert(k, normalize(v, policy));
            }
            Value::Object(out)
        }
        _ => v,
    }
}

fn project_fields(v: &Value, only: &[String]) -> Value {
    match v {
        Value::Object(map) => {
            let mut out = serde_json::Map::new();
            for k in only {
                if let Some(val) = map.get(k) {
                    out.insert(k.clone(), val.clone());
                }
            }
            Value::Object(out)
        }
        _ => v.clone(),
    }
}

fn remove_fields(v: &Value, ignore: &[String]) -> Value {
    match v {
        Value::Object(map) => {
            let mut out = serde_json::Map::new();
            for (k, v) in map.iter() {
                if ignore.iter().any(|x| x == k) { continue; }
                out.insert(k.clone(), remove_fields(v, ignore));
            }
            Value::Object(out)
        }
        Value::Array(arr) => Value::Array(arr.iter().map(|x| remove_fields(x, ignore)).collect()),
        _ => v.clone(),
    }
}

fn collect_diffs(path: &str, a: &Value, b: &Value, diffs: &mut Vec<String>, policy: &ComparePolicy) {
    use serde_json::Number;
    match (a, b) {
        (Value::Object(ma), Value::Object(mb)) => {
            for (k, va) in ma {
                let p = format!("{path}.{k}");
                if let Some(vb) = mb.get(k) {
                    collect_diffs(&p, va, vb, diffs, policy);
                } else {
                    diffs.push(format!("missing in new: {}", p));
                }
            }
            for (k, _) in mb.iter() {
                if !ma.contains_key(k) {
                    diffs.push(format!("extra in new: {path}.{k}"));
                }
            }
        }
        (Value::Array(va), Value::Array(vb)) => {
            if va.len() != vb.len() {
                diffs.push(format!("{} length old={} new={}", path, va.len(), vb.len()));
            }
            for (i, (ia, ib)) in va.iter().zip(vb.iter()).enumerate() {
                collect_diffs(&format!("{}[{}]", path, i), ia, ib, diffs, policy);
            }
        }
        (Value::Number(na), Value::Number(nb)) => {
            if let (Some(fa), Some(fb)) = (na.as_f64(), nb.as_f64()) {
                if let Some(eps) = policy.numeric_eps {
                    if (fa - fb).abs() > eps {
                        diffs.push(format!("{} numeric diff: {} vs {}", path, fa, fb));
                    }
                } else if na != nb {
                    diffs.push(format!("{} number diff: {} vs {}", path, na, nb));
                }
            } else if na != nb {
                diffs.push(format!("{} number diff: {} vs {}", path, na, nb));
            }
        }
        _ => {
            if a != b {
                diffs.push(format!("{} diff: {} vs {}", path, a, b));
            }
        }
    }
}
