program projectileMotion
implicit none
real:: u, a, g, H, R, T, x, y, i=0
character(len=100):: gnu_command
g = 9.8
write(*,*) "--------------------PROJECTILE MOTION--------------------"
write(*,*) "Enter the angle at which the projectile is fired:"
read(*,*) a
write(*,*) "Enter the initial velocity of the projectile:"
read(*,*) u
T = (2*u*sind(a))/g
write(*,*) "The time taken by the projectile to reach the ground is:", T, "secs"
H = (u*u*sind(a)*sind(a))/(2*g)
write(*,*) "The height covered by the projectile is:", H, "meters"
R = (u*u*sind(2*a))/g
write(*,*) "The Horizontal range of the projectile is:", R, "meters"

open(unit=10, file="projectiledata.dat", status="replace")
do while (i <= T)
x = u*cosd(a)*i
y = x*tand(a)-(g*x*X)/(2*u*u*cosd(a)*cosd(a))
write(10, '(2f10.5)') x,y
i = i+0.1
end do
x = u*cosd(a)*T
y = x*tand(a)-(g*x*X)/(2*u*u*cosd(a)*cosd(a))
write(10, '(2f10.5)') x,y
close(10)

open(unit=11, file="projectileplot.gp", status="replace")
write(11, '(a)') 'plot "projectiledata.dat" with linespoints'
write(11, '(a)') 'pause -1'
close(11)

gnu_command = 'gnuplot -p projectileplot.gp'
call system(gnu_command)

end program projectileMotion
