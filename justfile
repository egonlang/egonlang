default:
    @just --list

build:
    cargo build

build-release:
    cargo build --release

clean:
    cargo clean

cli *args:
    cargo run egon {{args}}

format:
    cargo fmt --all

lint:
    cargo fmt --all -- --check
    cargo clippy --all-targets --no-deps --workspace

verify:
    just test
    just lint

verify-with-logs:
    just verify 2>err.log 1>out.log

test *args:
    cargo nextest run --workspace {{args}}

clean-git-branches:
    git branch -d $(git branch --merged=main | grep -v main) && git fetch --prune
