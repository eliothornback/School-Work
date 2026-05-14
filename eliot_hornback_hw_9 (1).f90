!Name:Eliot Hornback
!Date:11.14.25
!Simpson's rule numerical integration code
!with double precision and subroutines
!Got best value of 993.63147788943058 at N=500

module simpsons_code
  implicit none
  integer, parameter :: dp = kind(1.0d0)
contains
  subroutine get_limits(x_low, x_hi)
    implicit none
    real(kind=dp), intent(out) :: x_low, x_hi  
    x_low = 1.0d0
    x_hi = 20.0d0
  end subroutine get_limits

  real(kind=dp) function integrand(x)
    implicit none
    real(kind=dp), intent(in) :: x
    real(kind=dp), parameter :: A = 4000.0d0, B = 15.5d0, C = 0.01d0

    integrand = ((x + cos(x)) * exp(cos(x))) + &
                A * exp(-((x-B)**2) / C)    
  end function integrand
end module simpsons_code

program eliot_hornback_hw_9
  use simpsons_code 
  implicit none

  integer :: N                                       !Number of subintervals
  integer :: i                                       !Loop index
  real(kind=dp) :: x_low, x_hi                       !Bounds
  real(kind=dp) :: dx                                !Subinterval width
  real(kind=dp) :: xi                                !x at ith subinterval
  real(kind=dp) :: sum                               !Holds sum var
  real(kind=dp) :: f_low, f_mid, f_hi                !Function var 
  
  write(*,*) "Enter number of subintervals(N) as an integer:"
  read(*,*) N

  call get_limits(x_low, x_hi)                       !Initalize x_low,x_hi
  
  dx = (x_hi - x_low) / real(N)                      !Finds dx width
  sum = 0.0d0                                        !Iniatilize sum              
  xi = x_low                                         !Initialize xi
               
  do i = 1, N
     f_low = integrand(xi)
     f_mid = integrand(xi + dx/2.0d0)
     f_hi = integrand(xi + dx)

     sum = sum + (f_low + 4.0d0*f_mid + f_hi) * dx/6.0d0
     xi = xi + dx
  enddo 

  write(*,*) "Sum=", sum
  stop 0 
end program eliot_hornback_hw_9
