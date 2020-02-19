#include <boost/lambda/lambda.hpp>
#include <iostream>
#include <fstream>
#include <boost/tokenizer.hpp>
#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>
#include <boost/graph/depth_first_search.hpp>
#include <deque>

/* Contains some customization over Boost graph */

using namespace boost;
using namespace std;

// Type for our graph
struct City {
  int id;
  int x, y;
};
struct Road { int length; };

typedef adjacency_list<vecS, vecS, undirectedS, City, Road> Graph;
typedef graph_traits<Graph>::edge_descriptor Edge;
typedef graph_traits<Graph>::vertex_descriptor Vertex;

// Allow us to write the vertex position in the output for graphviz
template <class Name>
class MyWriter {
  public:
    MyWriter(Name _name) : name(_name) {}
    template <class VertexOrEdge>
      void operator()(std::ostream& out, const VertexOrEdge& v) const {
        out << "[label=\"" << name[v].id << "\",";
        out << "pos=\"" << name[v].x << "," << name[v].y << "!\"]";
      }
  private:
    Name name;
};

// Add an offset between nodes
struct sample_graph_writer {
  void operator()(std::ostream& out) const {
    out << "K=3" << std::endl;
    out << "splines=true" << std::endl;
  }
};

float custom_distance(const City& c1,const City& c2) {
  float tmp;
  tmp = (c1.x - c2.x)*(c1.x - c2.x);
  tmp += (c1.y - c2.y)*(c1.y - c2.y);
  return sqrt(tmp);
}

/* A class for the closest pair algorithm */
class Chain {
  private:
    Vertex start;
    Vertex end;
    vector<Vertex> list;

  public:
    Chain(const Vertex& v): start(v), end(v){ 
      list.push_back(v);
    }

    int nb_elements() {
      return list.size();
    }

    Vertex& list_element(int i) {
      if (i < 0 && i >= list.size()) 
        cout << "Outside array" << endl;
      return list[i];
    }

    void add(const Vertex& v) {
      list.push_back(v);
    }
    void print(const Graph& g) {
      for (int i = 0; i < list.size(); ++i) 
        cout << g[list[i]].id << " ";
      cout << endl;
    }
    Vertex& end_point(int i) {
      if (i == 0) return list.front();
      return list.back();
    }

    void merge(const Chain& c, int extr1, int extr2) {
      list.insert(list.end(), c.list.begin(), c.list.end());

      // Change start if the new start is from the second
      if (extr1) start = c.start;
      if (extr2) end = c.end;
    }
};

