default:
    @just --list

build:
    cargo build

build-release:
    cargo build --release

clean:
    cargo clean

format:
    cargo fmt --all

lint:
    cargo fmt --all -- --check
    cargo clippy --all-targets --no-deps --workspace

verify:
    just test
    just lint

test *args:
    cargo nextest run --workspace {{args}}

clean-git-branches:
    git branch -d $(git branch --merged=main | grep -v main) && git fetch --prune
