module top (a, y);
input  a;
output y;
wire   n;
inv i0 (.a(a), .y(n));
buff i1 (.a(n), .y(y));
endmodule
