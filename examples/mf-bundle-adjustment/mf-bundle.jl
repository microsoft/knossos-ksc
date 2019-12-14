const Mat = Matrix{Float64}
const Vec = Vector{Float64}
const Vec3 = Vector{Float64}
const Vec2 = Vector{Float64}
const SparseMatrix = Array

struct Camera
    name :: string
    focal_length :: Real
    position :: Vec3
    orientation :: Vec3
end

struct SLAM
    cameras :: Array{Camera}
    points :: Array{Vec3}
end

function loss(params :: SLAM, data :: SparseMatrix{Vec2})

end

struct dCamera
    name :: ()
    focal_length :: Real
    position :: Vec3
    orientation :: Vec3
end

struct dSLAM
    cameras :: Array{dCamera}
    points :: Array{Vec3}
end

function grad_loss(params :: SLAM, data :: SparseMatrix{Vec2}) :: Pair{dSLAM, SparseMatrix{Vec2}}
