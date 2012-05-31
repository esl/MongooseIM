#!/usr/bin/env ruby

$summary      = "ct_report/cover.html"
$summary_data = "cover_summary.txt"

def make_cover_summary()
    open($summary, "w") do |cover|
        cover.write("<html>\n<head></head>\n<body bgcolor=\"white\" text=\"black\" link=\"blue\" vlink=\"purple\" alink=\"red\">\n")
        cover.write("<h1>Coverage for application 'omp'</h1>\n")
        cover.write("<table border=3 cellpadding=5>\n")
        cover.write("<tr><th>Module</th><th>Covered (%)</th><th>Covered (Lines)</th><th>Not covered (Lines)</th>")
        sum = open($summary_data, "r")
        c_sum = 0
        nc_sum = 0;
        while (line = sum.gets)
            cells = line.split(/;/)
            cover.write("<tr>")
            cover.write("<td><a href='../../rel/ejabberd/#{cells[3]}'>#{cells[0]}</a></td>")
            c = cells[1].to_f
            nc = cells[2].to_f
            p = c/(nc+c)*100
            c_sum = c_sum + c
            nc_sum = nc_sum + nc
            cover.write("<td>#{p.round} %</td>")
            cover.write("<td>#{cells[1]}</td>")
            cover.write("<td>#{cells[2]}</td>")
            cover.write("</tr>\n")
        end
        p = c_sum/(nc_sum + c_sum) * 100
        cover.write("<tr>")
        cover.write("<td>Summary</td>")
        cover.write("<td>#{p.round} %</td>")
        cover.write("<td>#{c_sum.round}</td>")
        cover.write("<td>#{nc_sum.round}</td>")
        cover.write("</tr>\n")
        cover.write("</table>\n</body>\n</html>")
        cover.close
    end
end

if __FILE__ == $0
    make_cover_summary()
end
