#!/usr/bin/env ruby

numThre = ARGV[0].to_i

ts = []
numThre.times{
  t = Thread.new{
    loop {
      mapFns = `ls -1 ../data/*.map`.split(/\n/) + `ls -1 ../smallmap/*.map`.split(/\n/)
      mapFns += `ls -1 ../mediummap/*.map`.split(/\n/) 
      fn = mapFns.sort_by{rand()}[0]
      `./dist/build/ll-ai-tuja/ll-ai-tuja  -i #{fn}`
    }
  }
  ts << t
}

ts.each{|t|
  t.join
}
