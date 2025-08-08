// 扫描 tests/cases 下的 *.try 脚本，运行并与快照比对
// 覆盖 CLI 的基础流程输出，确保回归敏感
use trycmd::TestCases;

#[test]
fn cli_cases() {
    // NOTE: 以当前 crate 为工作目录，.try 文件中的相对路径以该目录为基准
    TestCases::new()
        .case("tests/cases/*.trycmd");
}

