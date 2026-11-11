program sorting
implicit none
integer, parameter:: n = 5
integer:: x(n), i, temp, length = n
write(*,*) "Enter any five values:"
do i = 1, n
read(*,*) x(i)
end do
do while (length > 1)
	do i = 1, (n-1)
	if (x(i) > x(i+1)) then
	temp = x(i)
	x(i) = x(i+1)
	x(i+1) = temp
	end if
	end do
length = length - 1
end do
write(*,*) "Increasing order: ", x
end program sorting
