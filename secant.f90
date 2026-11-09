program secant
implicit none
real:: x0, x1, x2, tol, f, a=0
integer:: i
character(len=100):: gnu_command
write(*,*) "Enter the interval: "
read(*,*) x0, x1
write(*,*) "Enter Tolerance: "
read(*,*) tol
do i = 1, 100
x2 = x1 - f(x1)*(x1-x0)/(f(x1)-f(x0))
write (*,*) "Iteration: ", i, ", x =", x2
if (abs(x2-x1) < tol) then
write(*,*) "Root found: ", x2
exit
end if
x0 = x1
x1 = x2
if (i == 100) print *, "Root not found until 100 iterations."
end do

open(unit=10, file="secantdata.dat", status="replace")
do while (a <= 10)
write(10, '(2f10.5)') a, f(a)
a = a + 0.1
end do
close(10)

open(unit=11, file="secantplot.gp", status="replace")
write(11, '(a)') 'plot "secantdata.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(11)

gnu_command = 'gnuplot -p secantplot.gp'
call system(gnu_command)

end program secant

real function f(x)
implicit none
real:: x
f = x**3-x-1
end function f
