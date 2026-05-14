

!Name:Eliot Hornback
!Purpose: Calculate the brightness of a binary star system
!Date: 9.26.25

program eliot_hornback_hw_4
  implicit none

  real, parameter :: PI = 3.14159265 ! PI  constant
  real :: t, phase                   ! time phase variables
  real :: m_ap                       ! magnitude of brightness variable
  real, parameter :: period = 6.4    ! period
  integer :: n                       ! number of periods at time t
  

  ! Time input 
  write(*,*) "Enter time(days):"
  read(*,*) t 

  ! Convert t to phase value
  n = int(t / period) 
  phase = t - (n * period) 

  ! Piecewise
  if ((phase >= 0.0) .and. (phase < 0.9)) then
     m_ap = 2.5

  elseif ((phase >= 0.9) .and. (phase < 2.3)) then
     m_ap = 3.335 - log(1.352 + cos((PI * (phase - 0.9)) / 0.7))

  elseif ((phase >= 2.3) .and. (phase < 4.4)) then 
     m_ap = 2.5
  
  elseif ((phase >= 4.4) .and. (phase < 5.2)) then 
     m_ap = 3.598 - log(1.998 + cos((PI * (phase - 4.4)) / 0.4))

  elseif ((phase >= 5.2) .and. (phase < 6.4)) then
     m_ap = 2.5
  end if

  ! Display
  write(*,*) "After", t, "days, the magnitude is:", m_ap

  stop 0 
end program eliot_hornback_hw_4
