use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fortran_parser::parse;

fn bench_small_program(c: &mut Criterion) {
    let source = r#"
program hello
    implicit none
    integer :: i
    print *, 'Hello, World!'
end program hello
"#;
    
    c.bench_function("parse small program", |b| {
        b.iter(|| parse(black_box(source)))
    });
}

fn bench_medium_program(c: &mut Criterion) {
    let source = r#"
program calculate
    implicit none
    integer :: i, n, sum
    real :: x(100), average
    
    n = 100
    sum = 0
    average = 0.0
    
    do i = 1, n
        x(i) = real(i) * 2.5
        sum = sum + i
        if (x(i) > 50.0) then
            print *, 'Value > 50:', x(i)
        end if
    end do
    
    average = real(sum) / real(n)
    print *, 'Average:', average
end program calculate
"#;
    
    c.bench_function("parse medium program", |b| {
        b.iter(|| parse(black_box(source)))
    });
}

fn bench_expressions(c: &mut Criterion) {
    let source = r#"
program expr_test
    implicit none
    integer :: a, b, c
    a = 1 + 2 * 3 - 4 / 2
    b = (a + 1) ** 2
    c = a .gt. 5 .and. b .lt. 100
end program expr_test
"#;
    
    c.bench_function("parse expressions", |b| {
        b.iter(|| parse(black_box(source)))
    });
}

fn bench_loops(c: &mut Criterion) {
    let source = r#"
program loop_test
    implicit none
    integer :: i, j
    do i = 1, 10
        do j = 1, 10
            if (i * j > 50) then
                print *, i, j
            end if
        end do
    end do
end program loop_test
"#;
    
    c.bench_function("parse nested loops", |b| {
        b.iter(|| parse(black_box(source)))
    });
}

criterion_group!(benches, bench_small_program, bench_medium_program, bench_expressions, bench_loops);
criterion_main!(benches);

