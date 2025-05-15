module dup_check

    implicit none
    private
    public CheckDuplicate

    contains

    logical function CheckDuplicate(arr) result(has_duplicate)

        implicit none

        integer, intent(in) :: arr(:)
        integer :: i, j

        has_duplicate = .false.

        outer_loop: do i = 1, size(arr)
            inner_loop: do j = i+1, size(arr)

                if ( arr(i) == arr(j) ) then
                    has_duplicate = .true.
                    return
                end if 
                
            end do inner_loop
        end do outer_loop

    end function CheckDuplicate

end module dup_check

program ContainsDuplicate

    use dup_check

    implicit none

    

    integer, dimension(:), allocatable :: nums
    logical :: result
    integer :: n

    ! --- test input ---
    n = 7
    allocate(nums(n))
    nums = [1,2,3,4,5,1,6]

    ! --- test input ---

    result = CheckDuplicate(nums)

    if (result) then
        print *, "True"
    else
        print *, "False"
    end if

end program ContainsDuplicate

