from math import sqrt,floor

def find_nprocs_subgrids(grid_setup,np_max):
    nested_grid_x = grid_setup['nested_grid_x']
    nested_grid_y = grid_setup['nested_grid_y']
    orig_grid_x = grid_setup['orig_grid_x']
    orig_grid_y = grid_setup['orig_grid_y']
    dxgrid_orig = grid_setup['dxgrid_orig']
    dygrid_orig = grid_setup['dygrid_orig']
    dxgrid_nest = grid_setup['dxgrid_nest']
    dygrid_nest = grid_setup['dygrid_nest']
    nested_grid_start_x = grid_setup['nested_grid_start_x']
    nested_grid_start_y = grid_setup['nested_grid_start_y']    
    print('Possible solutions with number of processes (nprocs) and grid sizes (ip,jp):')
    print('') 
    for np in range(np_max,0,-1):
        b = nested_grid_y / nested_grid_x
        a_x = (nested_grid_x*(1-dxgrid_nest/dxgrid_orig) + orig_grid_x)        
        a_y = (nested_grid_y*(1-dygrid_nest/dygrid_orig) + orig_grid_y)
        a_xy = a_x*a_y
        a2 = a_xy/np
#         print(np,a_x,a_y,a2)
        if (a2==floor(a2)):   
#             print('Candidate:',a2, ' for np=',np)
            b_n = nested_grid_y / nested_grid_x     
            b_o = orig_grid_y / orig_grid_x
            if b_n < 1:
                b_n = 1/b_n
            if b_o < 1: 
                b_o = 1/b_o
            b = b_n
            if b_o > b:
                b = b_o
            if b<2:
                b=2                
            ip_guess = sqrt(a2)
            ip_b = floor(ip_guess/b)
            ip_e = int(ip_guess*b)
            for ip in range(ip_b,ip_e):
                jp = a2/ip
                nprocs_x = nested_grid_start_x/ip
                nprocs_y = nested_grid_start_y/jp
                rprocs_x = (orig_grid_x-(nested_grid_x*dxgrid_nest)/dxgrid_orig - nested_grid_start_x)/ip
                rprocs_y = (orig_grid_y-(nested_grid_y*dygrid_nest)/dygrid_orig - nested_grid_start_y)/jp
                if (jp==floor(jp)):
#                     print(np,' Valid dy:',dx,int(dy),';',nx,ny,rx,ry)
                        p_col = (orig_grid_x + nested_grid_x*(1 - dxgrid_nest/dxgrid_orig) ) / ip
                        p_row = (orig_grid_y + nested_grid_y*(1 - dygrid_nest/dygrid_orig) ) / jp
                        if (nprocs_x==floor(nprocs_x)) and (nprocs_y==floor(nprocs_y)) and \
                            (rprocs_x==floor(rprocs_x)) and (rprocs_y==floor(rprocs_y)) and \
                            (p_row==floor(p_row)) and (p_col==floor(p_col)) :
                            n_procs_nested_grid = int(p_col-nprocs_x-rprocs_x)*int(p_row-nprocs_y-rprocs_y)
                            n_procs_orig_grid = int(np - n_procs_nested_grid)
                            print('Number of processes, ip x jp, proc_cols x proc_rows') #, npx, npy')   
                            print(np,',',ip,'x',int(jp),',',int(p_col),'x',int(p_row))
                            print('procs in orig grid; (before+after) x (below+above) nested grid')
                            print(n_procs_orig_grid,';(',int(nprocs_x),'+',int(rprocs_x),')x(',int(nprocs_y),'+',int(rprocs_y),')')
                            print('procs in nested grid:')
                            print(n_procs_nested_grid,';',int(p_col-nprocs_x-rprocs_x),'x',int(p_row-nprocs_y-rprocs_y))
                            o2n = sqrt(dxgrid_orig*dygrid_orig/dxgrid_nest/dygrid_nest)
                            print('Number of cores needed:',int(n_procs_nested_grid+n_procs_orig_grid/o2n))
                            print('')
                            
# So now we need some heuristic to group processes from the orig grid.
# We assume that we can put sqrt(dxgrid_orig*dygrid_orig/dxgrid_nest/dygrid_nest) processes per cor
# because they are slower


grid_setup = {
'nested_grid_x' : 200,
'nested_grid_y' : 200,
'nested_grid_start_x' : 100,
'nested_grid_start_y' : 100,
'orig_grid_x' : 300,
'orig_grid_y' : 300,
'dxgrid_orig' : 4,
'dygrid_orig' : 4,
'dxgrid_nest' : 2,
'dygrid_nest' : 2
}

np_max  = 24 
                   
find_nprocs_subgrids(grid_setup, np_max)                            


grid_setup_Kyoto = {
'nested_grid_x' : 4000/2,
'nested_grid_y' : 1000/2,
'nested_grid_start_x' : 1000/4,
'nested_grid_start_y' : 800/4,
'orig_grid_x' : 12000/4,
'orig_grid_y' : 2600/4,
'dxgrid_orig' : 4,
'dygrid_orig' : 4,
'dxgrid_nest' : 2,
'dygrid_nest' : 2
}
np_max = 256

find_nprocs_subgrids(grid_setup_Kyoto, np_max)  
