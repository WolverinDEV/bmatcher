use bmatcher_core::{
    compiler::{
        self,
    },
    BinaryMatcher,
};
use criterion::{
    black_box,
    criterion_group,
    criterion_main,
    BenchmarkId,
    Criterion,
    Throughput,
};
use rand::{
    rngs::StdRng,
    RngCore,
    SeedableRng,
};

pub fn simple_binary_pattern(instance: &mut Criterion) {
    {
        let pattern = compiler::parse_pattern("FF FF 00 00 FF DE AD BE EF").unwrap();
        let pattern = compiler::optimize_pattern(&pattern);

        let mut group = instance.benchmark_group("binary");
        for target_size in [128usize, 1024, 1024 * 4, 1024 * 1024] {
            group.throughput(Throughput::Bytes(target_size as u64));
            group.bench_with_input(
                BenchmarkId::from_parameter(target_size),
                &target_size,
                |bencher, size| {
                    let mut buffer = Vec::with_capacity(*size);

                    let mut rng = StdRng::from_seed([0xEEu8; 32]);
                    buffer.resize_with(*size, || rng.next_u32() as u8);

                    let buffer = buffer.as_slice();
                    bencher.iter(|| {
                        let mut matcher =
                            BinaryMatcher::new(black_box(&pattern), black_box(&buffer));
                        while let Some(_match) = matcher.next_match() {
                            panic!("pattern should not match");
                        }
                    });
                },
            );
        }
        group.finish();
    }

    {
        let pattern = compiler::parse_pattern("FF FF 00 00 ? ? ? ? FF DE AD BE EF").unwrap();
        let pattern = compiler::optimize_pattern(&pattern);

        let mut group = instance.benchmark_group("binary with wildcard");
        for target_size in [128usize, 1024, 1024 * 4, 1024 * 1024] {
            group.throughput(Throughput::Bytes(target_size as u64));
            group.bench_with_input(
                BenchmarkId::from_parameter(target_size),
                &target_size,
                |bencher, size| {
                    let mut buffer = Vec::with_capacity(*size);

                    let mut rng = StdRng::from_seed([0xEEu8; 32]);
                    buffer.resize_with(*size, || rng.next_u32() as u8);

                    let buffer = buffer.as_slice();
                    bencher.iter(|| {
                        let mut matcher =
                            BinaryMatcher::new(black_box(&pattern), black_box(&buffer));
                        while let Some(_match) = matcher.next_match() {
                            panic!("pattern should not match");
                        }
                    });
                },
            );
        }
        group.finish();
    }
}

criterion_group!(benches, simple_binary_pattern);
criterion_main!(benches);
