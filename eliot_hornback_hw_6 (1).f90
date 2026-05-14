!Name:Eliot Hornback
!Date:10.9.25
!Simpson's rule numerical integration code
!  N       sum
!  10      294.583619
!  100     1005.96283
!  200     993.635498
!  300     993.630188
!  400     993.635559
!  500     993.630066
!  600     993.630371
!  700     993.638367

!Most accurate value of N is probably ~500N
!The sum converges to the number ~993.63
!Then starts to vary more as N increases

program eliot_hornback_hw_6
  implicit none

  integer :: N                                       !Number of subintervals
  integer :: i                                       !Loop index
  real, parameter :: A = 4000.0, B = 15.5, C = 0.01  !Given constants
  real :: x_low, x_hi                                !Bounds
  real :: dx                                         !Subinterval width
  real :: xi                                         !x at ith subinterval
  real :: sum                                        !Holds sum var
  real :: fi, fmid, fend                             !Function var
  real :: t1, t2, t3, t4, t5, t6                     !Function term vars
  
  write(*,*) "Enter number of subintervals(N) as an integer:"
  read(*,*) N

  write(*,*) "Enter lower  and upper bounds:"
  read(*,*) x_low, x_hi 
  
  dx = (x_hi - x_low) / real(N)                      !Finds dx width
  sum = 0.0                                          !Iniatilize sum
  xi = x_low                                         !Initialize xi

  do i = 1, N
     !fi
     t1 = (xi + cos(xi)) * exp(cos(xi))
     t2 = A * exp(-((xi - B)**2) / C)
     fi = t1 + t2

     !fmid
     t3 = ((xi + dx/2.0d0) + cos(xi + dx/2.0d0)) * exp(cos(xi + dx/2.0d0))
     t4 = A * exp(-(((xi + dx/2.0d0) - B)**2) / C)
     fmid = t3 + t4

     !fend
     t5 = ((xi + dx) + cos(xi + dx)) * exp(cos(xi + dx))
     t6 = A * exp(-(((xi + dx) - B)**2) / C)
     fend = t5 + t6

     sum = sum + (fi + 4.0d0*fmid + fend) * dx/6.0d0
     xi = xi + dx
  enddo 

  write(*,*) "Sum=", sum
  stop 0 
end program eliot_hornback_hw_6
