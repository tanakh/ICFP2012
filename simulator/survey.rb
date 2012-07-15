#!/usr/bin/env ruby

numThre = ARGV[0].to_i

$mapFns = `ls -1 ../data/*.map`.split(/\n/) + `ls -1 ../smallmap/*.map`.split(/\n/)
$mapFns += `ls -1 ../mediummap/*.map`.split(/\n/) 




def survey()
  log = `ls -1 record/*/*`.split(/\n/)
  
  
  total = 0
  xs0 = $mapFns.map{|fn|
    fnBody = fn.split('/')[-1].split('.')[0]
    cnt = 0
    log.each{|logfn|
      cnt += 1 if logfn[7..-1][0...fnBody.length] == fnBody
    }
    STDERR.print "#{fnBody} #{cnt}   "
    total += cnt
    [fn, cnt]
  }
  STDERR.print "\n"
  STDERR.print "total: #{total}\n"
  maxcnt = xs0.map{|fn,cnt| cnt}.max
  $launchInfo = xs0.map{|fn,cnt| [fn, (maxcnt-cnt).to_f]}
end

survey()

ts = []
numThre.times{
  t = Thread.new{
    loop {
      fn = $launchInfo.sort_by{|fn,cnt| cnt * rand()}[-1][0]
      system "./dist/build/ll-ai-tuja/ll-ai-tuja -s -i #{fn}"
    }
  }
  ts << t
}

loop{
  sleep 60
  survey()
}

ts.each{|t|
  t.kill
}
