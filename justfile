default:
    @just --list

# Build the project
build:
    cargo build
    cd vsc && just build
    cd egon-book && just build

# Build the project using release project
build-release:
    cargo build --release
    cd vsc && just build
    cd egon-book && just build

# Generate docs for cargo crates
docs *args:
    cargo doc --no-deps --workspace {{args}}

# Generate docs for cargo crates and open it in the browser
docs-open:
    just docs --open

# Clean all build artifacts
clean:
    cargo clean
    just clean-coverage
    cd vsc && just clean
    cd egon-book && just clean

# Run the Egon command
cli *args:
    cargo run --bin egon {{args}}

# Run the Egon command with trace logging enabled
cli-trace *args:
    just cli --features='tracelog' {{args}}

# Run the Egon language server
lsp:
    cargo run --bin egonlang-lsp

# Fix cargo issues
fix:
    cargo fix --allow-staged
    cargo clippy --fix --allow-staged

# Format cargo crates code
format:
    cargo fmt --all
    cargo clippy --fix --all-targets --no-deps --workspace

# Lint cargo crates code
lint:
    cargo fmt --all -- --check
    cargo clippy --all-targets --no-deps --workspace

# Run tests and lint across the project
verify:
    just test
    just lint

# Run tests and lint across the project with trace logging enabled
verify-trace:
    just test-trace
    just lint

# Run verify and dump the results in to out.log & err.log
[unix]
verify-with-logs:
    just verify 2>err.log 1>out.log

# Run tests across the project
test *args:
    cargo nextest run --workspace {{args}}
    cargo test --doc

# Run tests and gather coverage across the project
coverage *args:
    CARGO_INCREMENTAL=0 RUSTFLAGS='-Cinstrument-coverage' LLVM_PROFILE_FILE='cargo-test-%p-%m.profraw' cargo nextest run --workspace {{args}}
    grcov . --binary-path ./target/debug/deps/ -s . -t html --branch --ignore-not-existing --ignore '../*' --ignore "/*" -o target/coverage/html
    just clean-coverage

# Clean up coverage files
clean-coverage:
    find ./ -name "*.profraw" | xargs rm -r

# Run tests across the project with trace logging enabled
test-trace *args:
    just test --features='tracelog' --no-capture {{args}}

# Delete git branches already merged in to `main`
clean-git-branches:
    git branch -d $(git branch --merged=main | grep -v main) && git fetch --prune

build-docker:
    docker build -t egonlang/egonlang:0.1.0 .

run-docker +args:
    docker run --rm --read-only -v "/$PWD":/home/egon:ro -it egonlang/egonlang:0.1.0 egon {{args}}

# Serve the Egon book
serve-book:
    cd egon-book && just serve