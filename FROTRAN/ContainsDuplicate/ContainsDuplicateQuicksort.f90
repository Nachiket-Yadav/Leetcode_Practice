module dup_check_quicksort

    contains
    logical function contains_duplicate(arr) result(has_duplicate)
        implicit none

        integer, intent(inout) :: arr(:)
        integer :: i
        has_duplicate = .false.

        call sort_array(arr) !quicksort

        do i = 1,size(arr)-1

            if ( arr(i) == arr(i+1) ) then
                has_duplicate = .true.
                return
            end if 
        
        end do

    end function contains_duplicate

    subroutine sort_array(arr)
        implicit none 

        integer, intent(inout) :: arr(:)
        call quicksort(arr,1,size(arr))

    end subroutine sort_array

    recursive subroutine quicksort(arr,left,right)
        implicit none
        
        integer, intent(inout) :: arr(:)
        integer, intent(in) :: left, right
        integer :: i, j, pivot, temp

        if (left >= right) return

        pivot = arr((left + right)/2)
        i = left
        j = right

        do 
            do while (arr(i) < pivot)
                i = i + 1
            end do

            do while (arr(j) > pivot)
                j = j - 1
            end do

            if (i <= j) then 
                temp = arr(i)
                arr(i) = arr(j)
                arr(j) = temp

                i = i + 1
                j = j - 1
            end if 

            if (i > j) exit
        end do

        call quicksort(arr,left,j)
        call quicksort(arr,i,right)

    end subroutine quicksort 

end module dup_check_quicksort

program ContainsDuplicate

    use dup_check_quicksort

    implicit none

    

    integer, dimension(:), allocatable :: nums
    logical :: result
    integer :: n

    ! --- test input ---
    n = 7
    allocate(nums(n))
    nums = [1,2,3,4,5,1,6]

    ! --- test input ---

    result = contains_duplicate(nums)

    if (result) then
        print *, "True"
    else
        print *, "False"
    end if

end program ContainsDuplicate