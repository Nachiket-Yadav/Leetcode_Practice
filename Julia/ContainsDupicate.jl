function contains_duplicate(nums::Vector{Int})::Bool
    return length(Set(nums)) != length(nums)
end