! original subroutine name: press {
    do l = 1, 50, 1
        sor = 0.0
        do nrd = 0, 1, 1
            do k = 1, 80, 1
                do j = 1, 300, 1
                    do i = 1, 300, 1
                        dz1 = dzs(k-1)
                        dz2 = dzs(k)
                        cn4s = 2./(dz1*(dz1+dz2))
                        cn4l = 2./(dz2*(dz1+dz2))
                        cn3s = 2./(dys(j-1)*(dys(j-1)+dys(j)))
                        cn3l = 2./(dys(j)*(dys(j-1)+dys(j)))
                        cn2s = 2./(dxs(i-1)*(dxs(i-1)+dxs(i)))
                        cn2l = 2./(dxs(i)*(dxs(i-1)+dxs(i)))
                        cn1 = 1./(2./(dxs(i-1)*dxs(i))+2./(dys(j-1)*dys(j))+2./(dz1*dz2))
                        if (nrd==0) then
                            reltmp = 1.0*(cn1*(cn2l*p(0,i+1,j,k)+cn2s*p(0,i-1,j,k)+cn3l*p(0,i,j+1,k)+cn3s*p(0,i,j-1,k)+cn4l*p(0,i,j,k+1)+cn4s*p(0,i,j,k-1)-rhs(i,j,k))-p(0,i,j,k))
                            p(1,i,j,k) = p(0,i,j,k)+reltmp
                        else
                            reltmp = 1.0*(cn1*(cn2l*p(1,i+1,j,k)+cn2s*p(1,i-1,j,k)+cn3l*p(1,i,j+1,k)+cn3s*p(1,i,j-1,k)+cn4l*p(1,i,j,k+1)+cn4s*p(1,i,j,k-1)-rhs(i,j,k))-p(1,i,j,k))
                            p(0,i,j,k) = p(1,i,j,k)+reltmp
                        end if
                    end do
                end do
            end do
            do k = 0, 81, 1
                do j = 0, 301, 1
                    p(0,0,j,k) = p(0,1,j,k)
                    p(0,301,j,k) = p(0,300,j,k)
                end do
            end do
            do k = 0, 81, 1
                do i = 0, 301, 1
                    p(0,i,0,k) = p(0,i,300,k)
                    p(0,i,301,k) = p(0,i,1,k)
                end do
            end do
        end do
        do j = 0, 301, 1
            do i = 0, 301, 1
                p(0,i,j,0) = p(0,i,j,1)
                p(0,i,j,81) = p(0,i,j,80)
            end do
        end do
    end do
!}
