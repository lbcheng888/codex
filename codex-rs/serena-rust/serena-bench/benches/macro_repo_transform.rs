use criterion::{criterion_group, criterion_main, Criterion};
use ignore::WalkBuilder;
use rand::{rngs::StdRng, Rng, SeedableRng};
use std::fs;
use std::io::Write;
use std::path::PathBuf;
use std::time::Instant;
use sysinfo::{System, SystemExt, ProcessExt};

fn make_fixture(temp: &PathBuf, files: usize, lines: usize) -> anyhow::Result<()> {
    fs::create_dir_all(temp)?;
    for i in 0..files {
        let mut p = temp.clone();
        p.push(format!("file_{i}.txt"));
        let mut f = fs::File::create(p)?;
        for _ in 0..lines {
            writeln!(f, "This is a line with number {i} and some content.")?;
        }
    }
    Ok(())
}

fn transform_repo(root: &PathBuf) -> anyhow::Result<usize> {
    // 简单改造任务：遍历并进行字符串替换统计
    let mut touched = 0usize;
    for ent in WalkBuilder::new(root).build() {
        let ent = ent?;
        if !ent.file_type().map(|t| t.is_file()).unwrap_or(false) { continue; }
        let p = ent.path();
        let content = fs::read_to_string(p)?;
        if content.contains("line") {
            let newc = content.replace("line", "LINE");
            if newc != content { fs::write(p, newc)?; touched += 1; }
        }
    }
    Ok(touched)
}

fn bench_repo_transform(c: &mut Criterion) {
    let mut group = c.benchmark_group("macro_repo_transform");

    group.bench_function("cold_start_transform", |b| {
        b.iter_custom(|iters| {
            let mut total = std::time::Duration::ZERO;
            for _ in 0..iters {
                // 冷启动：新建临时仓库
                let dir = tempfile::tempdir().unwrap();
                let root = dir.path().to_path_buf();
                make_fixture(&root, 200, 50).unwrap();
                // 记录内存和时间
                let mut sys = System::new_all();
                sys.refresh_all();
                let me_pid = std::process::id() as i32;
                let start_mem = sys.process(me_pid).map(|p| p.memory()).unwrap_or(0);
                let t0 = Instant::now();
                let touched = transform_repo(&root).unwrap();
                let dt = t0.elapsed();
                sys.refresh_process(me_pid);
                let end_mem = sys.process(me_pid).map(|p| p.memory()).unwrap_or(0);
                // 把结果写入黑盒避免优化
                criterion::black_box((touched, end_mem - start_mem));
                total += dt;
            }
            total
        })
    });

    // 热启动：固定目录，重复改造（先写随机内容避免缓存完全命中）
    group.bench_function("hot_start_transform", |b| {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().to_path_buf();
        make_fixture(&root, 200, 50).unwrap();
        let mut rng = StdRng::seed_from_u64(42);
        b.iter(|| {
            // 轻微扰动
            let mut p = root.clone();
            p.push(format!("file_{}.txt", rng.gen_range(0..200)));
            let mut s = fs::OpenOptions::new().append(true).open(p).unwrap();
            writeln!(s, "hot path").unwrap();
            let touched = transform_repo(&root).unwrap();
            criterion::black_box(touched);
        })
    });

    group.finish();
}

criterion_group!(benches, bench_repo_transform);
criterion_main!(benches);
