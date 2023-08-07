src=$1

koka $src.kk -o a.out && chmod +x a.out && ./a.out && rm a.out

