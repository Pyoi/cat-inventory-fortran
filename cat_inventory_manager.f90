program CatInventoryManager
    implicit none

    character(len=*), parameter :: RED   = char(27)//'[31m'
    character(len=*), parameter :: RESET = char(27)//'[0m'
    character(len=*), parameter :: SAVEFILE = 'inventory.txt'

    ! 名前は日本語にしておきます。※うちのシャムは肉派、魚でも可
    character(len=*), parameter :: K_WET_BEEF    = 'ウェット牛'
    character(len=*), parameter :: K_WET_CHICKEN = 'ウェット鶏'
    character(len=*), parameter :: K_DRY_FOOD    = 'ドライ'
    character(len=*), parameter :: K_CAT_LITTER  = '猫砂'
    character(len=*), parameter :: K_UPDATED_AT  = '最終更新'

    integer :: wet_beef = 0
    integer :: wet_chicken = 0
    integer :: dry_food = 0
    integer :: cat_litter = 0

    integer :: choice, item, quantity
    logical :: running
    running = .true.

    call load_inventory_labeled_jp(SAVEFILE, wet_beef, wet_chicken, dry_food, cat_litter)

    do while (running)
        call display_inventory()

        print *, 'Select an action:'
        print *, '1 - Purchase (add)'
        print *, '2 - Consume (subtract)'
        print *, '3 - Exit'
        read (*,*) choice

        select case (choice)
        case (1, 2)
            print *, 'Select item:'
            print *, '1 - Wet Beef'
            print *, '2 - Wet Chicken'
            print *, '3 - Dry Food'
            print *, '4 - Cat Litter'
            read (*,*) item

            print *, 'Enter quantity:'
            read (*,*) quantity
            quantity = abs(quantity)

            if (choice == 1) then
                call update_inventory(item, quantity)
            else
                call update_inventory(item, -quantity)
            end if

            call save_inventory_labeled_jp(SAVEFILE, wet_beef, wet_chicken, dry_food, cat_litter)

        case (3)
            call save_inventory_labeled_jp(SAVEFILE, wet_beef, wet_chicken, dry_food, cat_litter)
            running = .false.

        case default
            print *, 'Invalid choice.'
        end select
    end do

contains

    subroutine display_inventory()
        print *, 'Current Inventory:'
        call print_item('Wet Beef   : ', wet_beef)
        call print_item('Wet Chicken: ', wet_chicken)
        call print_item('Dry Food   : ', dry_food)
        call print_item('Cat Litter : ', cat_litter)
        print *, ''
    end subroutine display_inventory

    subroutine print_item(label, value)
        character(len=*), intent(in) :: label
        integer, intent(in) :: value
        if (value <= 1) then
            write(*,'(a,a,i0,a)') trim(label), RED, value, RESET
        else
            write(*,'(a,i0)') trim(label), value
        end if
    end subroutine print_item

    subroutine update_inventory(item, delta)
        integer, intent(in) :: item, delta
        integer :: after
        select case (item)
        case (1)
            after = wet_beef + delta
            if (delta < 0 .and. after < 0) print *, 'Warning: consumption exceeded stock; set to 0.'
            wet_beef = max(0, after)
        case (2)
            after = wet_chicken + delta
            if (delta < 0 .and. after < 0) print *, 'Warning: consumption exceeded stock; set to 0.'
            wet_chicken = max(0, after)
        case (3)
            after = dry_food + delta
            if (delta < 0 .and. after < 0) print *, 'Warning: consumption exceeded stock; set to 0.'
            dry_food = max(0, after)
        case (4)
            after = cat_litter + delta
            if (delta < 0 .and. after < 0) print *, 'Warning: consumption exceeded stock; set to 0.'
            cat_litter = max(0, after)
        case default
            print *, 'Invalid item.'
        end select
    end subroutine update_inventory

    
    ! 日本語と、読み込みしっかりと
    

    subroutine load_inventory_labeled_jp(filename, wb, wc, df, cl)
        character(len=*), intent(in) :: filename
        integer, intent(inout) :: wb, wc, df, cl

        integer :: u, ios, n, eqpos
        logical :: exists
        character(len=256) :: line
        character(len=128) :: key, val

        wb = 0; wc = 0; df = 0; cl = 0

        inquire(file=filename, exist=exists)
        if (.not. exists) return

        open(newunit=u, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Warning: could not open save file. Starting with zeros.'
            return
        end if

        do
            read(u, '(A)', iostat=ios) line
            if (ios /= 0) exit

            line = adjustl(line)
            if (len_trim(line) == 0) cycle
            if (line(1:1) == '#' .or. line(1:1) == ';') cycle

            eqpos = index(line, '=')
            if (eqpos <= 1) cycle

            key = trim(adjustl(line(1:eqpos-1)))
            val = trim(adjustl(line(eqpos+1:)))

            ! 数値で読めない行は、飛ばします。

            read(val, *, iostat=ios) n
            if (ios /= 0) then
                ios = 0
                cycle
            end if

            select case (trim(key))
            case (K_WET_BEEF)
                wb = max(0, n)
            case (K_WET_CHICKEN)
                wc = max(0, n)
            case (K_DRY_FOOD)
                df = max(0, n)
            case (K_CAT_LITTER)
                cl = max(0, n)
            case default
                ! unknown key -> ignore
            end select
        end do

        close(u)
    end subroutine load_inventory_labeled_jp

    subroutine save_inventory_labeled_jp(filename, wb, wc, df, cl)
        character(len=*), intent(in) :: filename
        integer, intent(in) :: wb, wc, df, cl

        integer :: u, ios
        character(len=64) :: stamp

        call now_timestamp(stamp)

        open(newunit=u, file=filename, status='replace', action='write', iostat=ios)
        if (ios /= 0) then
            print *, 'Warning: could not write save file.'
            return
        end if

        write(u,'(A)') '# 猫の在庫 保存ファイル'
        write(u,'(A)') '# 形式: キー=整数'
        write(u,'(A)') trim(K_UPDATED_AT)//'='//trim(stamp)

        write(u,'(A)') trim(K_WET_BEEF)//'='    // trim(int_to_str(max(0,wb)))
        write(u,'(A)') trim(K_WET_CHICKEN)//'=' // trim(int_to_str(max(0,wc)))
        write(u,'(A)') trim(K_DRY_FOOD)//'='    // trim(int_to_str(max(0,df)))
        write(u,'(A)') trim(K_CAT_LITTER)//'='  // trim(int_to_str(max(0,cl)))

        close(u)
    end subroutine save_inventory_labeled_jp

    subroutine now_timestamp(out)
        character(len=*), intent(out) :: out
        integer :: values(8)
        character(len=32) :: buf
        call date_and_time(values=values)
        ! values: (1)year (2)month (3)day (5)hour (6)min (7)sec
        write(buf,'(i4.4,"-",i2.2,"-",i2.2," ",i2.2,":",i2.2,":",i2.2)') &
            values(1), values(2), values(3), values(5), values(6), values(7)
        out = trim(buf)
    end subroutine now_timestamp

    function int_to_str(x) result(buf)
        integer, intent(in) :: x
        character(len=32) :: buf
        write(buf, '(i0)') x
    end function int_to_str

end program CatInventoryManager