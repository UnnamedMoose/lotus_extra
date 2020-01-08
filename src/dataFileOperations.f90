!***********************************************************************
!>
!! Module for importing externally generated data stored in text files.
!!
!! Version: 1.0
!! Author: Artur K. Lidtke (artur.lidtke@gmail.com)
!!
!! CHANGELOG
!! v 1.0 - original implementation
!!
!<
!***********************************************************************
module dataFileOperations
	implicit none

! - Module-level variables. -
private

! - Public function declarations. -
public readTabulatedData

contains

!>
!! Reads the specified file and returns data within as a 2D array.
!<
subroutine readTabulatedData(filename, data)
    ! Input related variables
    character(len=*), intent(in) :: filename
    character(len=100) :: buffer
    integer :: positionInBuffer
    integer :: fileUnit
    integer :: ios=0
    integer :: line=0, arrayIndex=0
    logical :: arrayLengthAllocated=.false.

    ! data variables
    integer :: sizeI, sizeJ
    real*8, dimension(:,:), allocatable, intent(out) :: data

    ! other
    integer :: errorCode

    ! open the input stream
    open(newunit=fileUnit, file=filename)

    ! keep reading until the EOF
    do while (ios == 0)
        ! read the next line into the buffer, check the error status
        read(fileUnit, '(A)', iostat=ios) buffer

        ! if got a valid line
        if (ios == 0) then
            ! increment the line counter
            line = line + 1

            ! if this is not a comment line
            if (scan(buffer, '#') == 0) then

                ! see if the size of the array has been allocated or not
                if ((.not. arrayLengthAllocated) .and. (scan(buffer, 'size') >= 1)) then
                    ! check where the first delimiting space is
                    positionInBuffer = scan(buffer, ' ')

                    ! discard the keyword part
                    buffer = buffer(positionInBuffer+1:)

                    ! read the size of the array and allocate it
                    read(buffer, *, iostat=ios) sizeI, sizeJ
                    allocate(data(sizeI, sizeJ), stat=errorCode)

                    ! do not try to reallocate the size
                    arrayLengthAllocated = .true.

                ! if this is a data line
                else if (arrayLengthAllocated) then
                    ! move to the next line in the array
                    arrayIndex = arrayIndex + 1

                    if (arrayIndex <= sizeI) then
                        read(buffer, *, iostat=ios) data(arrayIndex, :)
                    else
                        write(0, *) "Index exceeds array dimension! Check array size in ", &
                        "the input file ", filename
                    end if
                end if
            end if
        end if
    end do
end subroutine readTabulatedData

end module dataFileOperations
