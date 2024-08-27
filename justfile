default:
    @just --list

build:
    cargo build
    cd vsc && just build
    cd egon-book && just build

build-release:
    cargo build --release
    cd vsc && just build
    cd egon-book && just build

docs:
    cargo doc --no-deps --workspace

docs-open:
    cargo doc --no-deps --workspace --open

clean:
    cargo clean
    just clean-coverage
    cd vsc && just clean
    cd egon-book && just clean

cli *args:
    cargo run --bin egon {{args}}

cli-trace *args:
    just cli --features='tracelog' {{args}}

lsp:
    cargo run --bin egonlang-lsp

format:
    cargo fmt --all
    cargo clippy --fix --all-targets --no-deps --workspace

lint:
    cargo fmt --all -- --check
    cargo clippy --all-targets --no-deps --workspace

verify:
    just test
    just lint

verify-trace:
    just test-trace
    just lint

verify-with-logs:
    just verify 2>err.log 1>out.log

test *args:
    cargo nextest run --workspace {{args}}
    cargo test --doc

coverage *args:
    CARGO_INCREMENTAL=0 RUSTFLAGS='-Cinstrument-coverage' LLVM_PROFILE_FILE='cargo-test-%p-%m.profraw' cargo nextest run --workspace {{args}}
    grcov . --binary-path ./target/debug/deps/ -s . -t html --branch --ignore-not-existing --ignore '../*' --ignore "/*" -o target/coverage/html
    just clean-coverage

clean-coverage:
    find ./ -name "*.profraw" | xargs rm -r

test-trace *args:
    just test --features='tracelog' --no-capture {{args}}

clean-git-branches:
    git branch -d $(git branch --merged=main | grep -v main) && git fetch --prune

build-docker:
    docker build -t egonlang/egonlang:0.1.0 .

run-docker +args:
    docker run --rm --read-only -v "/$PWD":/home/egon:ro -it egonlang/egonlang:0.1.0 egon {{args}}

serve-book:
    cd egon-book && just serve