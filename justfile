default:
    @just --list

build:
    cargo build
    cd vsc && just build

build-release:
    cargo build --release

clean:
    cargo clean

cli *args:
    cargo run {{args}}

cli-trace *args:
    just cli --features='verify-trace' {{args}}

format:
    cargo fmt --all

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

test-trace *args:
    just test --features='verify-trace' --no-capture {{args}}

clean-git-branches:
    git branch -d $(git branch --merged=main | grep -v main) && git fetch --prune

build-docker:
    docker build -t egonlang/egonlang:0.1.0 .

run-docker +args:
    docker run --rm --read-only -v "/$PWD":/home/egon:ro -it egonlang/egonlang:0.1.0 egon {{args}}
