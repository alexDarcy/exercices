SRC=$(wildcard *.cpp)
OBJ=$(SRC:.cpp=.o)
CPP=g++
	
tsp: ${OBJ}
	${CPP} -o $@ $<

exec: tsp run plot

run:
	./tsp

# We need fdp for ordering points
plot:
	dot graph_init.dat -Kfdp -Tsvg > graph_init.svg
	dot graph_nearest.dat -Kfdp -Tsvg > graph_nearest.svg
	dot graph_closest.dat -Kfdp -Tsvg > graph_closest.svg

clean:
	rm -f *.o
