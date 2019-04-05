
subroutine dyn_2_smart_cache
      real :: h_read_in
      real :: un_read_in
      real :: vn_read_in
      real, dimension(1:1005) :: h_buffer
      real, dimension(1:1005) :: un_buffer
      real, dimension(1:1005) :: vn_buffer
      real :: h_jm1_k
      real :: h_j_kp1
      real :: h_j_k
      real :: h_jp1_k
      real :: h_j_km1
      real :: un_j_k
      real :: un_j_km1
      real :: vn_j_k
      real :: vn_jm1_k
      integer :: i
      integer :: count
      integer :: compIndex
      integer, parameter :: nloop = 503
      integer, parameter :: smartCacheSize = 1005
      integer, parameter :: maxPositiveOffset = 503
      integer, parameter :: maxNegativeOffset = 503
    do count = 0, count<nloop, 1
        compIndex = count-maxPositiveOffset
        !$PRAGMA unroll
        do i = 0, i<smartCacheSize-1, 1
                h_buffer(i) = h_buffer(i+1)
                un_buffer(i) = un_buffer(i+1)
                vn_buffer(i) = vn_buffer(i+1)
        end do
        if (count<smartCacheSize) then
                ! F4 Comment: read pipe dyn_2_h_j_k_reader__dyn_2_smart_cache__h_j_k__pipe
                call readPipe(dyn_2_h_j_k_reader__dyn_2_smart_cache__h_j_k__pipe, h_read_in)
                h_buffer(1004) = h_read_in
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: read pipe dyn_1__dyn_2_smart_cache__un_j_k__pipe
                call readPipe(dyn_1__dyn_2_smart_cache__un_j_k__pipe, un_read_in)
                un_buffer(1004) = un_read_in
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: read pipe dyn_1__dyn_2_smart_cache__vn_j_k__pipe
                call readPipe(dyn_1__dyn_2_smart_cache__vn_j_k__pipe, vn_read_in)
                vn_buffer(1004) = vn_read_in
                call memFence(CLK_CHANNEL_MEM_FENCE)
        end if
        if (compIndex>=0) then
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__h_j_k__pipe
                h_j_k = h_buffer(503)
                call writePipe(dyn_2_smart_cache__dyn_2__h_j_k__pipe, h_j_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__h_j_km1__pipe
                h_j_km1 = h_buffer(1)
                call writePipe(dyn_2_smart_cache__dyn_2__h_j_km1__pipe, h_j_km1)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__h_j_kp1__pipe
                h_j_kp1 = h_buffer(1005)
                call writePipe(dyn_2_smart_cache__dyn_2__h_j_kp1__pipe, h_j_kp1)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__h_jm1_k__pipe
                h_jm1_k = h_buffer(502)
                call writePipe(dyn_2_smart_cache__dyn_2__h_jm1_k__pipe, h_jm1_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__h_jp1_k__pipe
                h_jp1_k = h_buffer(504)
                call writePipe(dyn_2_smart_cache__dyn_2__h_jp1_k__pipe, h_jp1_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__un_j_k__pipe
                un_j_k = un_buffer(1005)
                call writePipe(dyn_2_smart_cache__dyn_2__un_j_k__pipe, un_j_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__un_j_km1__pipe
                un_j_km1 = un_buffer(503)
                call writePipe(dyn_2_smart_cache__dyn_2__un_j_km1__pipe, un_j_km1)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__vn_j_k__pipe
                vn_j_k = vn_buffer(1005)
                call writePipe(dyn_2_smart_cache__dyn_2__vn_j_k__pipe, vn_j_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
                ! F4 Comment: write pipe dyn_2_smart_cache__dyn_2__vn_jm1_k__pipe
                vn_jm1_k = vn_buffer(1004)
                call writePipe(dyn_2_smart_cache__dyn_2__vn_jm1_k__pipe, vn_jm1_k)
                call memFence(CLK_CHANNEL_MEM_FENCE)
        end if
    end do
end subroutine dyn_2_smart_cache
