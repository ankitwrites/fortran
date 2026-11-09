program areaofcircle
implicit none
real:: r, pi, area
pi = 3.14159
write(*,*) "Enter radius:"
read(*,*) r
area = pi*r*r
write(*,*) "Area of the circle is: ", area
end program areaofcircle
