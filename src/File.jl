module OS

export getfqdn
"""
Result of readchomp(`hostname -f`)
"""
getfqdn() = readchomp(`hostname -f`)

export getusername
"""
readchomp(`id -u -n`)
"""
getusername() = readchomp(`id -u -n`)

end

# =============== Pid ====================

module Pid

import OS

debinfofun(on::Bool) = on ? (x...)->info(x...) : (x...)->nothing

statinfofun(on::Bool) = on ? (x...)->info(x...) : (x...)->nothing


export isworks_local
"""
isworks_local(pid::AbstractString; deb::Bool=false) -> Bool
"""
function isworks_local(pid::AbstractString; deb::Bool=false)
    debinfo::Function = debinfofun(deb)
    try cmd = `bash -c "
        ( ps -A | grep $pid ) >/dev/null && echo YES || echo NO"`
        cmdrv = readchomp(cmd)
        debinfo("""isworks_local($pid): Command '$cmd' 
        - returns '$cmdrv' """)
        cmdrv == "YES"
    catch e
        warn("isworks_local(\"$pid\"): ", e)
        return false
    end
 
end



export isworks_remote
"""
isworks_remote(pid::AbstractString,
                    host::AbstractString;
                                        deb::Bool=false) -> Bool
"""
function isworks_remote(pid::AbstractString, 
            host::AbstractString; 
            deb::Bool=false)
    debinfo::Function = debinfofun(deb)     
    cmd=`ssh $host "
        ( ps -A | grep $pid ) >/dev/null && echo YES || echo NO"`
    try cmdrv = readchomp(cmd)
        debinfo("""isworks_remote($pid,$host): Command '$cmd' 
        -  returns '$cmdrv' """)
        cmdrv == "YES"
    catch e 
        warn("isworks_remote($pid,$host): ", e)
        return false
    end

end                 


export is_local_host
"""
is_local_host(host::AbstractString) -> Bool
host must be a FQDN, like a result of \`hostname -f\` bash command.
"""
is_local_host{S<:AbstractString}(host::S) = host==OS.getfqdn()



export isworks
"""
isworks{S<:AbstractString}(pid::S, host::S; deb::Bool=false) -> Bool
"""
function isworks{S<:AbstractString}( pid::S, host::S; deb::Bool=false )

    if !ismatch(r"^\d+$",pid)
    error("pid can contain only digits (given pid='$pid')")
    end
    debinfo::Function = debinfofun(deb)
    if is_local_host(host)
    debinfo("""isworks:(\"$pid\",$host): - my own host.""")
    return isworks_local(pid, deb=deb)
    else
    debinfo("""isworks(\"$pid\",$host): - remote host.""") 
    return isworks_remote(pid, host, deb=deb)
    end 
end


end # module

# ======================== Flag ==========================

module Flag

import OS
import Pid

debinfofun(on::Bool) = on ? (x...)->info(x...) : (x...)->nothing

statinfofun(on::Bool) = on ? (x...)->info(x...) : (x...)->nothing


export name
"""
name(\"aa.txt\")      -> \"aa.txt.FLAG\"
name(\"aa.txt.FLAG\") -> \"aa.txt.FLAG\"
"""
function name(f::AbstractString)
 if !ismatch(r"\.FLAG$", f)
    f = string(f,".FLAG")
 end
 f 
end


export read
"read(filename) -> Dict"
function read(f::AbstractString)
    f = name(f)
    di=Dict{AbstractString,AbstractString}()
    if isfile(f)
    content = open(readchomp,f)
    if isempty(content)
        return di
    end    
    kva::Array = split(content,';')
    
    for kv in kva
        kv = split(kv,'=')
        if length(kv)<2
        continue
        end
        get!(di,kv[1],kv[2])
    end
    end 
    di
end    





export isworks
"""
isworks(\"aa.txt\", deb=false)
or the same: isworks(\"aa.txt.FLAG\", deb=false)
"""
function isworks(f, deb::Bool=false)
 debinfo = debinfofun(deb)
 fl = name(f)
 if isfile(fl)
    debinfo("set: File $fl - is exists.")
    if filesize(fl)>1000
    error("Size of flag file $fl > 1000 bytes")
    end
    flag::Dict = Flag.read(fl)
    debinfo("Flag file $fl content: $flag")
    pid = get(flag,"PID","")
    if isempty(pid)
    warn("empty 'pid' key in flag $fl, unset flag...")
    Flag.unset(fl)
    return false
    end 
    if Pid.isworks(pid, flag["HOSTFQDN"], deb=deb)
    return true
    end
 end
 false
end


export set
"""
set(filename::AbstractString; deb::Bool=false) -> Bool
"""
set(f::AbstractString; deb::Bool=false) = set(f, 
            Dict("PID"=>getpid(),
            "HOSTFQDN"=>OS.getfqdn(),
            "USER"=>OS.getusername(),
            "STARTED"=>now()),
            deb=deb)

function set(f::AbstractString, d::Dict; deb::Bool=false)
 debinfo::Function = debinfofun(deb)
 fl = name(f)
 if isworks(fl)
    return false
 end
 open(fl,"w") do io
    str = join(["$k=$v" for (k,v) in d], ";")
    debinfo("Write to $fl: \"$str\"")
    print(io,str)
 end    
 true    
end


export unset
"""
unset(filename::AbstractString) -> Bool
"""
function unset(f::AbstractString)
 f = name(f)
 if isfile(f)
    try rm(f)
    catch e 
    warn("unset: can't unset $f:", e)
    return false
    end
    return true
 else
    false
 end
end             
    

export via
"""
Set flag name, call func() and then unset flag.

via( func::Function, flagfile::String, extrargs... )

via( flagfile::String, extrargs... ) do name, extrargs...
    dosmf(...)
end
"""
function via(fun::Function, flag::AbstractString, extrargs...)
 if !Flag.set(flag)
    return false
 end    
 rv = fun(flag, extrargs...)
 Flag.unset(flag)
 rv    
end


end # module


# ============================= File ======================

module File

import Flag

export goodsize
"goodsize( filename ) -> Bool"
goodsize(f::AbstractString) = goodsize(f, Dict(".gz"=>20))


"""goodsize( filename, dict ) -> Bool
Define your own dict::Dict{String,Int} to redefine compared filesises.
"""
function goodsize{Str<:AbstractString}(
            f::AbstractString, dict::Dict{Str,Int})
    ext = splitext(f)[2];
    compare_with = get(dict, lowercase(ext), 0)
    filesize(f) > compare_with
end    


export iswait
"iswait( filename ) -> Bool"
function iswait{S<:AbstractString}(f::S)
    fl = Flag.name(f)
    if Flag.isworks(fl)
    return true
    end
    false       
end

export istodo
"istodo( filename ) -> Bool"
istodo{S<:AbstractString}(f::S) = 
    !goodsize(f) && !iswait(f)

"istodo( filename, dict ) -> Bool"
istodo{S<:AbstractString}(f::S, dict::Dict{S,Int}) = 
    !goodsize(f, dict) && !iswait(f)


export tmpname
"""
    tmpname(\"aa.txt\")      -> \"aa.txt.TMP\"
    tmpname(\"aa.txt.TMP\") -> \"aa.txt.TMP\"
"""
function tmpname(f::AbstractString)
 if !ismatch(r"\.TMP$", f)
    f = string(f,".TMP")
 end
 f 
end



export files

"""
    files(dir)
    
    Recursive seach all files in directory dir.
"""
function files(dir::AbstractString)
 f = x->true
 files(dir, @inline(f))
end

"""
    files(dir, filter::Function)
    
    Recursive search files in directory dir, filtered by filter.
"""
files( dir::AbstractString, only::Function ) = 
    @task _files(true, dir, only)

"""
    files(dir, re::Regex, args...)

    Recursive seach files in directory dir where files matched by re.
"""
files( dir::AbstractString, re::Regex, args... ) = 
    files(dir, x->ismatch(re,x), args... )

"files(dir, filter::Function, re::Regex, args...)"
files( dir::AbstractString, f1::Function, re::Regex, args...) =
    files(dir, x->f1(x)&&ismatch(re,x), args... )

"files(dir, flt1::Function, flt2::Function, ff::Function...)"    
files(dir::AbstractString, f1::Function, f2::Function, ff::Function...) =
 files(dir, x->f1(x)&&f2(x), ff...)

function _files(::Bool, dir::AbstractString, only::Function )
  if !isdir(dir) error("$dir not a directory") end
  for x in readdir(dir)
    pathtox = joinpath(dir,x) 
    if only(pathtox)
    produce(pathtox)
    end 
    if isdir(pathtox)
        _files(true, pathtox, only)
    end
  end
end


#---------------------- hasnear ------
export hasnear
"""
    Returns true if any existing name 
    in the dir of <file> are matches all of [re]
    and optional <test> returns true.

    hasnear(\"a.txt\", [r\".*txt\"])
    hasnear(\"a.txt\", r\".*txt\", f->isfile(f))
"""
function hasnear( file::AbstractString, 
                rr::Array{Regex,1},
                tst::Function=goodsize )
    (dir,basename)=splitdir(file)
    if isempty(dir)
    dir="."
    end 
    if isempty(basename)
        warn("has_near(\"$file\",$rr): Empty basename in \"$file\"")
        return false
    end
    basenames = readdir(dir)
    for r in rr
    if any( bn->ismatch(r,bn)&&tst(joinpath(dir,bn)) ,basenames )
        return true
    end    
    end
    false
end



"""
    Returns true if any existing name 
    in the dir of <file> are matches with <re>.

    hasnear( file::String, base_name::Regex, [test::Function] )

    hasnear(\"a.txt\", r\".*txt\")
    hasnear(\"a.txt\", r\".*txt\", f->isfile(f))
    \"a.txt\" |> hasnear(...) 

"""
hasnear( file::AbstractString, r::Regex ) =
                hasnear(file, [r])

hasnear( file::AbstractString, r::Regex, tst::Function ) =
                hasnear(file, [r], tst)

"hasnear(basename::Regex) -> file->hasnear(file,[basename])"
hasnear( r::Regex ) = f->hasnear(f,[r])

"hasnear(bn::Regex, tst::Func) -> file->hasnear(file,[bn],tst)"
hasnear( r::Regex, tst::Function ) = f->hasnear(f,[r],tst)

"hasnear(nn::Array{Regex,1}) -> file->hasnear(file,[nn])"
hasnear( rr::Array{Regex,1} ) = f->hasnear(f,rr)

"hasnear(nn::Array{Regex,1}, t::Func) -> file->hasnear(file,[nn],t)"
hasnear( rr::Array{Regex,1}, t::Function ) = f->hasnear(f,rr,t)

# ---------- with string: -----------
"""
    hasnear( file::String, bname::String, [tst::Function] )
    
    Is there <basename> near the <file> and tst(fullname) returns true ?
"""
function hasnear( file::AbstractString, 
           bname::AbstractString, 
           tst::Function=goodsize ) 
    
    (origdir,origbname) = splitdir(file)
    if bname==origbname
    warn("""hasnear(\"$file\",\"$bname\",...): new basename (\"$bname\") equal old base name.""")
    end
        
    tst(joinpath(origdir,bname))
end

"""
    hasnear( file::String, change::Function, [tst::Function=goodsize] )
    
    Gets newname via change(basename) of <file> 
    and returns result of tst(newname).
"""
function hasnear( file::AbstractString, 
           change::Function, 
           tst::Function=goodsize )

 (origdir,origbname) = splitdir(file)
 newbname = change(origbname)
 if newbname==origbname
    warn("""hasnear(\"$file\",...): new basename (\"$newbname\") has not difference whith old base name.""")
 end    
 tst( joinpath(origdir,newbname))
end 
            

end # module