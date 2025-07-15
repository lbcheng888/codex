// Lightweight stub used in environments where the real seatbelt sandbox
// binary is not available (such as CI/test runs).  The main agent code only
// requires the `PATH_TO_SEATBELT_EXECUTABLE` constant to exist, so we export a
// dummy value here to satisfy the import without pulling in any native
// dependencies.

export const PATH_TO_SEATBELT_EXECUTABLE = "";

