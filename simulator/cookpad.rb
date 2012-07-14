#!/usr/bin/env ruby

bestConfs = []

`ls -1 record/`.split(/\n/).each{|dirn|
  cands = []
  `ls -1 record/#{dirn}/*`.split(/\n/).each{|fn|
    open(fn,'r'){|fp|
      lines = fp.read.split(/\n/)
      cands << [lines[0].to_i, lines[2]]
    }
  }
  ranking = cands.sort_by{|score, connfig| -score}
  ranking[0..(ranking.length/10)].each{|score, conf|
    bestConfs << conf
  }
}


open("AI/LearnedConfig.hs",'w'){|fp|
  fp.puts <<HS
module AI.LearnedConfig where
import AI.Cooking

learnedConfigs :: [Config]
learnedConfigs = [#{bestConfs.join(',')}]
HS
}

