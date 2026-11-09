program factorial
implicit none
integer:: i, n
double precision:: fact
fact = 1
write(*,*) "Enter n:"
read(*,*) n
do i = 1, n
fact = fact*i
end do
write(*,*) "The factorial is:", fact
end program factorial
