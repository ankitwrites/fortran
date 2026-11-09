program bisection
implicit none
real:: x1, x2, m, f, tol, a=0
integer:: i
character(len=100):: gnu_command
write(*,*) "Enter Interval:"
read(*,*) x1, x2
write(*,*) "Enter Tolerance:"
read(*,*) tol
if (f(x1)*f(x2) < 0) then
print *, "Root lies within the interval."
do i = 1, 100
m = (x1+x2)/2
print *, "Iteration: ", i
print *, "x1: ", x1, "Mid-point: ", m, "x2: ", x2
if (abs(f(m)) < tol .or. abs(x2-x1) < tol) then
print *, "Root found: ", m
exit
end if
if (i == 100) print *, "Root was not found within 100 iterations."
if (f(x1)*f(m) < 0) then
x2 = m
else
x1 = m
end if
end do
else
print *, "Root does not lie within the interval."
end if

open(unit=10, file="bisectiondata.dat", status="replace")
do while (a <= 10)
write(10, '(2f10.5)') a, f(a)
a = a + 0.1
end do
close(10)

open (unit=11, file="bisectionplot.gp", status="replace")
write(11, '(a)') 'plot "bisectiondata.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(10)

gnu_command = 'gnuplot -p bisectionplot.gp'
call system(gnu_command)

end program bisection

real function f(x)
implicit none
real:: x
f = x**3-4*x-9
end function f
