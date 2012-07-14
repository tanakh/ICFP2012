#!/usr/bin/env ruby
n = ARGV[0].to_i

10.times{|t|
  w = n+rand(n)
  h = n+rand(n)
  
  ar = (0...h).map{|y| (0...w).map{|x| "."}}

  pats = [" ", "."," ", ".", "\\", "*", "#"].map{|c| [c,rand]}

  50.times{
    char = pats.sort_by{|c,hind|hind*rand}[0][0]
    rate  = 10 ** (-1-2 * rand)
    p1 = 628 * rand
    p2 = 628 * rand
    kx1 = 10 ** (0.5 -2 * rand ) * (2*rand-1)
    ky1 = 10 ** (0.5 -2 * rand ) * (2*rand-1)
    kx2 = 10 ** (0.5 -2 * rand ) * (2*rand-1)
    ky2 = 10 ** (0.5 -2 * rand ) * (2*rand-1)
    (0...h).each{|y| 
      (0...w).each{|x|
        val = Math::sin(x*kx1 + y*ky1 + p1) * Math::sin(x*kx2 + y*ky2 + p2)
        ar[y][x] = char if val > 1-rate
      }
    }
  }

  (0...h).each{|y| 
    ar[y][0] = "#"
    ar[y][w-1] = "#"
  }
  (0...w).each{|x| 
    ar[0][x] = "#"
    ar[h-1][x] = "#"
  }
  
  100.times{|ctr|
    x = 2+rand(w-4)
    y = 2+rand(h-4)
    if ar[y][x] == " " || ctr > 90
      ar[y-1][x] = "R"
      ar[y-0][x] = "\\"
      ar[y+1][x] = "L"
      break
    end
  }

  open("randmap#{n}-#{t}.map",'w'){|fp|
    fp.puts ar.map{|xs|xs.join}.join("\n")
  }
}
