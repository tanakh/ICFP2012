#!/usr/bin/env ruby

require 'timeout'

pid = fork{
  exec(ARGV.join(" "))
}

begin
  timeout(150){
    Process.wait(pid)
  }
rescue Timeout::Error
  Process.kill(2,pid)
end
