$hash = Hash.new
$items = []

def knapsack(file)
  knapsack_size, item_count, items, *_ = parseKnapsackData(file)
  $items = items
  max_value = solve(item_count,knapsack_size)
  puts max_value
end

def solve(i,x)
  if i == 0 || x == 0
    return 0
  else
    v = $items[i-1][0]
    w = $items[i-1][1]
    val1 = $hash["#{i-1},#{x}"] || solve(i-1,x)
    if x-w < 0
      $hash["#{i},#{x}"] = val1
      return val1
    else
      val2 = $hash["#{i-1},#{x-w}"]
      if val2
        val2 += v
      else
        val2 = solve(i-1,x-w) + v
      end
      val = [val1,val2].max
      $hash["#{i},#{x}"] = val
      return val
    end
  end
end

def parseKnapsackData(file)
  header, *body = File.readlines(file)
  knapsack_size = header.split[0].to_i
  item_count = header.split[1].to_i
  items = body.map { |line| line.split.map { |item| item.to_i }}
  return [knapsack_size, item_count, items]
end

knapsack("./src/Course3/knapsack1.txt")
knapsack("./src/Course3/knapsack_big.txt")
