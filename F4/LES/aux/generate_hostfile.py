#!/usr/bin/env python

from math import floor

# Verbose
VV=False

# Process matrix
procPerCol = 4
procPerRow = 4

# Processes per node for the nested grid
ppn_nest = 4
# Processes per node for the original grid
ppn_orig = 8

# Original grid size
orig_grid_y = 300 
orig_grid_x = 300

# Nested grid location and size
nested_grid_start_x = 100 
nested_grid_start_y = 100 
nested_grid_x = 200 
nested_grid_y = 200 

nested_grid_end_x  = nested_grid_x + nested_grid_start_x
nested_grid_end_y  = nested_grid_y + nested_grid_start_y
# Nest grid resolution
dxgrid_nest = 2
dygrid_nest = 2
dxgrid_orig = 4
dygrid_orig = 4

# Subgrid size
ipmax =orig_grid_x + nested_grid_x*(1 - (dxgrid_nest/dxgrid_orig))
jpmax = orig_grid_y + nested_grid_y*(1 - (dygrid_nest/dygrid_orig))
ip = ipmax / procPerCol # rows per process
jp = ipmax / procPerRow # columns per process
# Subgrid coordinates for nest
i_s_nest_start =  nested_grid_start_x / ip 
i_s_nest_end =  i_s_nest_start + nested_grid_x / ip - 1 
j_s_nest_start =  nested_grid_start_y / ip
j_s_nest_end =  j_s_nest_start + nested_grid_y / ip - 1

def inNestedGridByRank(local_rank):
            (i_s,j_s) = calcSubgridCoords(local_rank)
#            print(i_s,j_s)
#            print (i_s_nest_start ,i_s_nest_end ,j_s_nest_start ,j_s_nest_end)
            in_grid = (local_rank > 0) and i_s >= i_s_nest_start and i_s <= i_s_nest_end and j_s >= j_s_nest_start and j_s <= j_s_nest_end
            return in_grid

def calcSubgridCoords(local_rank):
        i_s = floor(local_rank / procPerRow)
        j_s = local_rank % procPerRow
        return (i_s,j_s)        

hosts = [0]*procPerCol*procPerRow
# if a process is not in the nested part, 
host_ctr = 0
prev_was_orig = False
for rank in range(0,procPerCol*procPerRow):
#    print(rank)
#    hosts[host_ctr]=0
    if (inNestedGridByRank(rank)):
        if prev_was_orig and hosts[host_ctr]-ppn_nest>0:
            host_ctr+=1
        prev_was_orig = True
        if hosts[host_ctr]>=ppn_nest:
            host_ctr+=1
        hosts[host_ctr]+=1
        if VV:
            print('nest: add',rank,'to host',host_ctr, hosts[host_ctr])
    else:
        if not prev_was_orig and hosts[host_ctr]-ppn_orig>0:
            host_ctr+=1
        prev_was_orig = True
        if hosts[host_ctr]>=ppn_orig:
            host_ctr+=1
        hosts[host_ctr]+=1
        if VV:
            print('orig: add',rank,'to host',host_ctr, hosts[host_ctr])
        
        
print('# Hostfile for nested grid')
host_ctr=0
for occ in hosts:
    if (occ>0):
        print('host'+str(host_ctr)+':'+str(occ))
    host_ctr+=1



