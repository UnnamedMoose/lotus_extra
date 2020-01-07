program test03
    use unitTests ! Module for unit testing.

    use dataFileOperations

    implicit none

    real(8), dimension(:,:), allocatable :: readData
    character(len=*), parameter :: dataFileName = "data.dat"

    logical :: pass
    integer :: i
    real(8) :: rmsError

    integer, dimension(2) :: answer_shape=(/8, 4/)
    real(8), dimension(8,4) :: answer_data
    answer_data(:,1) = (/0.0, 0.071428571, 0.142857143, 0.214285714, 0.285714286, 0.357142857, 0.428571429, 0.5/)
    answer_data(:,2) = (/0.35, 0.358844236, 0.377917877, 0.395866493, 0.408875427, 0.412695843, 0.400620502, 0.370631712/)
    answer_data(:,3) = (/-0.24, -0.228776308, -0.196992186, -0.147729581, -0.087355424, -0.027068084, 0.016519661, 0.031296677/)
    answer_data(:,4) = (/-37.6, -49.22256979, -48.85648738, -43.06037263, -32.89075468, -17.97078554, -1.82374146, 10.06653496/)

    ! - Read the data. -
    call readTabulatedData("./testData/" // dataFileName, readData)

    ! ---
    ! Test the shape.
    call assert((size(readData(:,1))==answer_shape(1)) .and. (size(readData(1,:))==answer_shape(2)), name="Shape", value=pass)
    if (.not. pass) then
        print "(2(a,i0))", "Read data of shape: ", size(readData(:,1)), ", ", size(readData(1,:))
    endif

    ! ---
    ! Test the contents.
    do i=1, 4
        rmsError = sqrt(sum((readData(:,i) - answer_data(:,i))**2.0))
        call assert(rmsError < 1e-5, name="Data column", value=pass)
        if (.not. pass) then
            print "(a,i0,a,e10.3)", "Data column ", i, " has rms error of ", rmsError
            print "(10F8.3)", readData(:,i)
        endif
    enddo

    ! ---
    call finishUnitTest

    ! - Clean up. -
    deallocate(readData)

end program test03
