set terminal png size 800,600
set output 'test05_Euler.png'

set termoption enhanced
set xlabel 'Time [s]' enhanced
set ylabel 'Displacement [m]' enhanced
set key right top

plot "<sed '1d' test05_Euler.dat" using ($1):($2) t "Euler-y0" axes x1y1 with lines lc rgb "red" lt 1 lw 2 , \
     "<sed '1d' test05_Euler.dat" using ($1):($3) t "Euler-y1" axes x1y1 with lines lc rgb "blue" lt 1 lw 2 , \
     "<sed '1d' test05_Euler.dat" using ($1):($8) t "Analytical-y0" axes x1y1 lc rgb "red" lt 6 lw 1 , \
     "<sed '1d' test05_Euler.dat" using ($1):($9) t "Analytical-y1" axes x1y1 lc rgb "blue" lt 6 lw 1
