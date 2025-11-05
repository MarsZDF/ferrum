use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fortran_lexer::{tokenize, Format};

fn bench_small_program(c: &mut Criterion) {
    let source = r#"
program hello
    implicit none
    integer :: i
    print *, 'Hello, World!'
end program hello
"#;
    
    c.bench_function("lex small program", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_medium_program(c: &mut Criterion) {
    let source = r#"
program matrix_multiply
    implicit none
    integer, parameter :: n = 100
    real :: a(n, n), b(n, n), c(n, n)
    integer :: i, j, k
    
    do i = 1, n
        do j = 1, n
            c(i, j) = 0.0
            do k = 1, n
                c(i, j) = c(i, j) + a(i, k) * b(k, j)
            end do
        end do
    end do
    
    print *, 'Matrix multiplication complete'
end program matrix_multiply
"#;
    
    c.bench_function("lex medium program", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_keyword_recognition(c: &mut Criterion) {
    let source = "program subroutine function module if then else do end integer real complex character logical";
    
    c.bench_function("lex keywords", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

fn bench_string_literals(c: &mut Criterion) {
    let source = r#"'string1' "string2" 'with''quote' "with""quote" 'another string literal here'"#;
    
    c.bench_function("lex string literals", |b| {
        b.iter(|| tokenize(black_box(source), Format::FreeFormat))
    });
}

criterion_group!(benches, bench_small_program, bench_medium_program, bench_keyword_recognition, bench_string_literals);
criterion_main!(benches);

