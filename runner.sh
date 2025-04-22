set -e

echo "COMPILING $1"

PREFIX="x86_64-linux-gnu"
## preprocessor
gcc -E -P $1 -o $1.i


## run the compiler
cargo run -- $1.i 

$PREFIX-gcc $2 --sysroot=/usr/x86_64-redhat-linux/sys-root/fc41/ out.s -o $1.compiled

echo "CLEANUP"

if [[ $2 == "-c" ]]; then
    mv $1.compiled $1.compiled.o;
fi

# rm $1.i