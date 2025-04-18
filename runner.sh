set -e

echo "COMPILING $1"

PREFIX="x86_64-linux-gnu"
## preprocessor
gcc -E -P $1 -o $1.i


## run the compiler
cargo run -- $1.i 

$PREFIX-gcc --sysroot=/usr/x86_64-redhat-linux/sys-root/fc41/ out.s -o $1.compiled

echo "CLEANUP"

# rm $1.i