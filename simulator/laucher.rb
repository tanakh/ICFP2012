#!/usr/bin/env ruby

pid = spawn("./dist/build/ll-ai-bf/ll-ai-bf -i ../officialmap/contest10.map -v -o conf.txt")

sleep 3

Process.kill(2,pid)
