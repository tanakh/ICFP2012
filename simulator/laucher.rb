#!/usr/bin/env ruby

require 'timeout'

pid = fork{
  exec("./dist/build/ll-ai-bf/ll-ai-bf -i ../officialmap/contest10.map -v -o conf.txt")
}

begin
  timeout(1){
    Process.wait(pid)
  }
rescue Timeout::Error
  Process.kill(2,pid)
end
