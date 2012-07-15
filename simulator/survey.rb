#!/usr/bin/env ruby

loop {
  mapFns = `ls -1 ../data/*.map`.split(/\n/) + `ls -1 ../smallmap/*.map`.split(/\n/)
  fn = mapFns.sort_by{rand()}[0]
  `./dist/build/ll-ai-tuja/ll-ai-tuja  -i #{fn}`
}
