!Author: Eliot Hornback
!Date: 11/7/25
!Purpose: Poisson equation charge distribution solution
!Got a value of 0.0 for q(25,5)
!Makes sense

program eliot_hornback_hw_8
  implicit none

  integer, parameter :: NX = 100           !No. cells in row
  integer, parameter :: NY = 100           !No. cells in col.
  real, parameter :: H = 0.01              !Cell size
  real, dimension(0:NY+1, 0:NX+1) :: q     !Charge density
  real, dimension(0:NY+1, 0:NX+1) :: unew  !New est. pot.
  real, dimension(0:NY+1, 0:NX+1) :: u     !Old est. of pot.
  real :: max_change = 1.0                 !Max change
  integer :: niter = 0                     !No. iterations
  integer :: i, j                          !Loop indices
  real, parameter :: PI = 3.141592654      !Pi
  real :: x, y                             !Cell cntr. coord.
  integer :: lun1                          !LUN for I/O

  u = 0.0       
  unew = u

  q = 0.0
  q(25,25) = -4.0
  q(75,75) = 4.0

  iter_loop: do while(max_change > 1.0e-5)
     do i=1,NY
        do j=1,NX
           unew(i,j) = (u(i+1,j) + u(i-1,j) + u(i,j+1) &
                + u(i,j-1) + 4.0*PI*(H**2) * q(i,j)) / 4.0
        enddo
     enddo

     max_change = maxval(abs(u-unew))
     write(*,*) "Maximum change =", max_change

     u = unew

     niter = niter+1

  enddo iter_loop

  write(*,*) "It took", niter,"iterations to converge"

  open(newunit=lun1, file="poisson.dat",&
       action="WRITE", status="replace")

  x = 0.5*H
  do j=0, NX+1
     y = 0.5 * H
     do i=0,NY+1
        write(LUN1, *) x,y,unew(i,j)
        y = y + H
     enddo
     write(lun1,*) "  "
     x = x + H

  enddo

  close(unit=lun1)

  write(*,*) "q(25,50) =", q(25,50)
  stop 0
end program eliot_hornback_hw_8
