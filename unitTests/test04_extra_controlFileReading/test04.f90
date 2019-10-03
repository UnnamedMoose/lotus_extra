program test04
    use unitTests ! Module for unit testing.

    use controlFileInterface

    implicit none

    integer :: answer_nLines=3, answer_int=9000, int
    real(8) :: answer_float=40.0, float
    real(8), dimension(3) :: answer_vector=(/0.3456, 0.0, 0.5/), vector
    logical :: pass

    type(controlFileClass) :: inputFile

    ! - Create and read the iput file. -
    inputFile = controlFileClass("./testData/controlInputs.dat")

    ! ---
    ! Test no. lines.
    call assert(inputFile%nLines==answer_nLines, name="Length of file")

    ! ---
    ! Test int - value in the file
    int = inputFile%getInt("int")
    call assert(int==answer_int, name="Read an int", value=pass)
    if (.not. pass) then
        print "(2(a,i0))", "Expected ", answer_int, " but got ", int
    endif
    ! Value not in the file.
    int = inputFile%getInt("intThatIsNotThere", suppressKeyNotFoundWarnig=.true.)
    call assert(int==-1, name="Integer not found", value=pass)
    if (.not. pass) then
        print "(2(a,i0))", "Expected ", -1, " but got ", int
    endif

    ! ---
    float = inputFile%getFloat("float")
    call assert(float==answer_float, name="Read a float", value=pass)
    if (.not. pass) then
        print "(2(a,F8.4))", "Expected ", answer_float, " but got ", float
    endif
    ! Value not in the file.
    float = inputFile%getFloat("floatThatIsNotThere", suppressKeyNotFoundWarnig=.true.)
    call assert(isnan(float), name="Float not found", value=pass)
    if (.not. pass) then
        print "(3a,F8.4)", "Expected ", "NaN", " but got ", float
    endif

    ! ---
    vector = inputFile%getVector("vec")
    call assert(sqrt(sum((answer_vector - vector)**2.0)) < 1e-6, name="Read a vector", value=pass)
    if (.not. pass) then
        print "(2(a,3F8.4))", "Expected ", answer_vector, " but got ", vector
    endif
    ! Value not in the file.
    vector = inputFile%getVector("vecThatIsNotThere", suppressKeyNotFoundWarnig=.true.)
    call assert((isnan(vector(1)) .and. isnan(vector(2)) .and. isnan(vector(3))), name="Vector not found", value=pass)
    if (.not. pass) then
        print "(3a,3F8.4)", "Expected NaN but got ", vector
    endif

    ! ---
    call finishUnitTest

end program test04
