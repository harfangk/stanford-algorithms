def count_comparison(filepath)
  array = File.readlines(filepath).map { |s| s.to_i }
  first_array = array.dup
  second_array = array.dup
  third_array = array.dup
  puts "First as pivot: "
  puts first_as_pivot(first_array)
  puts "Last as pivot: "
  puts last_as_pivot(second_array)
  puts "Median as pivot: "
  puts median_as_pivot(third_array)
end

def first_as_pivot(array)
  if array.length < 2
    return 0
  end
  if array.length == 2
    return 1
  end
  left, right = partition(array)
  return array.length - 1 + first_as_pivot(left) + first_as_pivot(right)
end

def last_as_pivot(array)
  if array.length < 2
    return 0
  end
  if array.length == 2
    return 1
  end
  pivot = array[array.length - 1]
  array[array.length - 1] = array[0]
  array[0] = pivot
  left, right = partition(array)
  return array.length - 1 + last_as_pivot(left) + last_as_pivot(right)
end

def median_as_pivot(array)
  if array.length < 2
    return 0
  end
  if array.length == 2
    return 1
  end
  median_candidates = [0, (array.length - 1) / 2, array.length - 1].map do
    |i| [i, array[i]]
  end
  median = median_candidates.sort {|x,y| x[1] <=> y[1]}[1]
  array[median[0]] = array[0]
  array[0] = median[1]
  left, right = partition(array)
  return array.length - 1 + median_as_pivot(left) + median_as_pivot(right)
end

def partition(array)
  if array.length < 2
    return array
  end
  pivot = array[0]
  i = 1
  j = 1
  while j < array.length
    if array[j] <= pivot
      v = array[i]
      array[i] = array[j]
      array[j] = v
      i += 1
    end
    j += 1
  end
  v = array[i-1]
  array[i-1] = array[0]
  array[0] = v
  if i > 1
    left = array.slice(0..(i-2))
  else
    left = []
  end
  right = array.slice(i..(array.length))
  return [left,right]
end
