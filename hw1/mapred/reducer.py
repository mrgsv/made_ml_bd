#! /usr/bin/python3
import sys

m_i = 0
v_i = 0
c_i = 0

for line in sys.stdin:
    c_j, m_j, v_j = map(float, line.split("\t"))
    v_i = (c_i * v_i + c_j * v_j) / (c_i + c_j) \
        + c_i * c_j * ((m_i - m_j) / (c_i + c_j)) ** 2
    m_i = (c_i * m_i + c_j * m_j) / (c_i + c_j)
    c_i += c_j

print(
    str(c_i), 
    str(m_i), 
    str(v_i), 
    sep="\t"
)
