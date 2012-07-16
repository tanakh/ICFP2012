#!/usr/bin/env ruby

require 'thread'
require 'timeout'

numThre = ARGV[0].to_i

$mapFns = []
$mapFns += `ls -1 ../officialmap/*.map`.split(/\n/) 
$mapFns += `ls -1 ../smallmap/*.map`.split(/\n/)
$mapFns += `ls -1 ../mediummap/randmap32-*.map`.split(/\n/) 
$mapFns += `ls -1 ../largemap/*-0.map`.split(/\n/) 
$mapFns += `ls -1 ../largemap/*-1.map`.split(/\n/) 
$mapFns += `ls -1 ../largemap/*-3.map`.split(/\n/) 

q = Queue.new

tids = []


`mkdir -p conf`
$mapFns.each{|mapfn|
  [2,4].each{|greedyDepth|
    [10,8,2,12].each{|bfDepth|
      ['True', 'False'].each{|first|
        conffn = "conf/#{bfDepth}-#{first}-#{greedyDepth}"
        open(conffn,'w'){|fp| fp.puts <<CONF
# Oracle
# Configuration File.
# Only The Fourth Line is important.
fromList [("bfDepth","#{bfDepth}"),("combineBFFirst","#{first}"),("greedyDepth","#{greedyDepth}")]
CONF
          q.push([conffn, mapfn])
        }
      }
    }
  }
}

numThre.times{|i|
  tid = Thread.start {
    myNum = i
    while task = q.pop
      puts "TID #{myNum} will perform #{task}"
      conf,fn = task

      pid = fork{
        exec("./dist/build/ll-ai-bf/ll-ai-bf -i #{fn}  -o #{conf}")
      }
      begin
        timeout(120){
          Process.wait(pid)
        }
      rescue Timeout::Error
        Process.kill(2,pid)
      end
    end
  }
  tids << tid
}


8.times{q.push(nil)}

tids.each{|tid| tid.join }
