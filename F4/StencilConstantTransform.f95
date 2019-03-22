do j = 0, 301, 1
    do i = 0, 301, 1
        p(0,i,j,0) = p(0,i,j,1)
        p(0,i,j,81) = p(0,i,j,80)
    end do
end do


do synIdx1 = 0, 1, 1
    do j = 0, 301, 1
        do i = 0, 301, 1
            do synIdx2 = 0, 81, 1
                if (synIdx1 == 0 .and. (synIdx2 == 0 .or. synIdx2 == 80))
                    p(synIdx1,i,j,synIdx2) = p(synIdx1,i,j,synIdx2+1)
                    p(synIdx1,i,j,synIdx2+1) = p(synIdx1,i,j,synIdx2)
                end if
            end do
        end do
     end do
end do
