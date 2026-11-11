program sumofn
implicit none
real:: sum
integer:: n, i
sum = 0
write(*,*) "Enter n:"
read(*,*) n
do i = 1, n
sum = sum + i
end do
write(*,*) "Sum upto N numbers is:", sum
end program sumofn
