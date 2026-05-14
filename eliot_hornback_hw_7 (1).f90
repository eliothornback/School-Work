  !Name:Eliot Hornback
  !Date:10.23.25
  !Purpose: calculate mean, median, standard deviation of dataset

program eliot_hornback_hw_7
  implicit none

  integer :: lun1, lun2                       !LUN var
  integer :: i, k, j                          !Loop indices
  integer :: ierror = 0                       !Holds ierror var 
  integer :: npts = 0                         !Number of datapts in infile
  real, allocatable :: data(:)                !Holds datapoints as array
  character(len=256) :: filename, outfile
  real :: sum = 0.0
  real :: sumsq = 0.0
  real :: mean, stddev, med
  real :: tempvar
  integer :: min_idx

  !User input filename
  write(*,*) "Enter the filename:"
  read(*,*) filename 

  !Determine npts
  open(newunit=lun1,file=trim(filename),status="OLD",iostat=ierror)
  if(ierror /= 0) then
     write(*,*) "Could not open file:("
     stop 1
  end if

  do while(ierror == 0) 
     if(ierror == 0) then
        npts = npts + 1
     else
        exit
     end if
  enddo

  allocate(data(npts))
  rewind(lun1)

  !Read data into array
  do i=1, npts, 1
     read(lun1,*) data(i)
  enddo
  
  close(unit=lun1)
  
  !Mean
  do i=1, npts
     sum = sum + data(i)
  enddo
  mean = sum / real(npts)

  !Compute stddev
  do i=1 ,npts
     sumsq = sumsq + (data(i) - mean)**2
  enddo
  stddev = sqrt(sumsq / npts)

  !Selection sort
  do i=1, npts - 1
     min_idx = i
     do j = i+1, npts
        if (data(j) < data(min_idx)) min_idx = j
     enddo
     if (min_idx /= i) then
        tempvar = data(i)
        data(i) = data(min_idx)
        data(min_idx) = tempvar
     end if
  end do

  !Median
  if (mod(npts,2) == 1) then
     med = data((npts /2) + 1)
  else
     med = 0.5 * (data(npts/2) + data((npts/2) + 1))
  end if

  !User input outfile
  write(*,*) "Enter output filename:"
  read(*,*) outfile

  !Open outfile
  open(newunit=lun2, file=trim(outfile), status="REPLACE", &
       action="WRITE",iostat=ierror)
  write(lun2,*) "Number of datapoints:", npts
  write(lun2,*) "Mean:", mean
  write(lun2,*) "Standard deviation:", stddev
  write(lun2,*) "Sorted Data:"
  do i=1, npts
     write(lun2,*) data(i)
  enddo
  close(lun2)

  !Display to terminal
  write(*,*) "Number of datapoints:", npts
  write(*,*) "Mean:", mean
  write(*,*) "Median:", med
  write(*,*) "Standard deviation:", stddev
  write(*,*) "Sorted Data:"
  do i=1, npts
     write(*,*) data(i)
  end do
  
  stop 0
end program eliot_hornback_hw_7
