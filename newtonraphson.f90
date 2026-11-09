program newtonraphson
implicit none
real:: x0, x1, f, df, tol, a
integer:: i
character(len=100):: gnu_command
a = 0
write(*,*) "Enter Initial Guess: "
read(*,*) x0
write(*,*) "Enter Tolerance:"
read(*,*) tol
do i = 1, 100
x1 = x0 - f(x0)/df(x0)
print *, "Iteration: ", i
print *, "x", i, ":", x1
if(abs(x1-x0) < tol) then
	print *, "Root found: ", x1
	exit
end if
if (i == 100) print *, "Root not found until 100 iterations"
x0 = x1
end do

open(unit=10, file="newtonraphsondata.dat", status="replace")
do while (a <= 10)
write(10, '(2f10.5)') a, f(a)
a = a + 0.1
end do
close(10)

open(unit=11, file="newtonraphsonplot.gp", status="replace")
write(11, '(a)') 'plot "newtonraphsondata.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(11)

gnu_command = 'gnuplot -p newtonraphsonplot.gp'
call system(gnu_command)
end program newtonraphson

real function f(x)
implicit none
real:: x
f = x**2-8*x+11
end function f

real function df(x)
implicit none
real:: x
df = 2*x-8
end function df
