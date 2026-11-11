program simpsonsthreebyeight
implicit none
real:: a, b, h, f, sum=0, n=0
integer:: i
real, dimension(13):: x, y
character(len=100):: gnu_command
write(*,*) "Enter lower and upper limit: "
read(*,*) a, b
h = (b-a)/12
do i = 1, 13
if(a<=b) then
x(i) = a
y(i) = f(x(i))
if ((i-1)==0 .or. (i-1)==12) then
sum = sum + y(i)
else if(mod((i-1), 3) == 0) then
sum = sum + 2*y(i)
else
sum = sum + 3*y(i)
end if
end if
a = a + h
end do
write(*,*) "The integration using simpsons 3/8 rule is: ", 3*h*sum/8
open(unit=10, file="simpsonsthreebyeight.dat", status="replace")
do while (n <= 10)
write(10, '(2f10.5)') n, f(n)
n = n + 0.1
end do
close(10)

open(unit=11, file="simpsonsthreebyeight.gp", status="replace")
write(11, '(a)') 'plot "simpsonsthreebyeight.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(10)

gnu_command = 'gnuplot -p simpsonsthreebyeight.gp'
call system(gnu_command)
end program simpsonsthreebyeight

real function f(x)
implicit none
real:: x
f = (1-8*x**3)**2
end function f
