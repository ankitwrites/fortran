program simpsonsonebythree
implicit none
real:: a, b, sum=0, h, f, n=0
integer:: i
real, dimension(13):: x, y
character(len=100):: gnu_command
write(*,*) "Enter lower and upper limit:"
read(*,*) a, b
h = (b-a)/12
do i = 1, 13
if (a <= b) then
x(i) = a
y(i) = f(x(i))
if ((i-1)==0 .or. (i-1)==12) then
sum = sum + y(i)
else if (mod((i-1),2) == 0) then
sum = sum + 2*y(i)
else
sum = sum + 4*y(i)
end if
a = a + h
end if
end do
write(*,*) "Integration by using Simpons 1/3 rule is: ", h*sum/3

open(unit=10, file="simpsonsonebythree.dat", status="replace")
do while (n <= 10)
write(10, '(2f10.5)') n, f(n)
n = n + 0.1
end do
close(10)

open(unit=11, file="simpsonsonebythreeplot.gp", status="replace")
write(11, '(a)') 'plot "simpsonsonebythree.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(10)

gnu_command = 'gnuplot -p simpsonsonebythreeplot.gp'
call system(gnu_command)
end program simpsonsonebythree

real function f(x)
implicit none
real:: x
f = 1/(1+x**2)
end function f
