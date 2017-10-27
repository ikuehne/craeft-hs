fn unsigned_lte(U64 a, U64 b) -> U1 {
    return a <= b;
}

fn signed_lte(I64 a, I64 b) -> U1 {
    return a <= b;
}

fn float_lte(Float a, Float b) -> U1 {
    return a <= b;
}

fn double_lte(Double a, Double b) -> U1 {
    return a <= b;
}

fn ptr_lte(U8 *a, U8 *b) -> U1 {
    return a <= b;
}


