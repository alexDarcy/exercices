#include "custom_graph.h"


/* Create a graph with random vertices */
void init_graph(Graph &g) {
  // Create the vertices
  Vertex u;
  for (int j = 0; j < 10; ++j) {
    u = add_vertex(g);
    // Bad random generator
    g[u].id = j;
    g[u].x = rand() % 10 + 1;
    g[u].y = rand() % 10 + 1;
  }

  // Create the edges
  Edge e1;
  float tmp;

  // Create edges
  graph_traits<Graph>::vertex_iterator v, i, next, end;
  for (tie(v, end) = vertices(g); v != end; ++v){
    int nb_edges = 0;
    for (tie(i, end) = vertices(g); i != end; ++i){
      if (i == v) break;
      tmp = custom_distance(g[*v], g[*i]);

      e1 = (add_edge(*v, *i, g)).first;
      g[e1].length = tmp;
      nb_edges++;
    }
  }
}

/* Create a queue from graph */
void init_queue(const Graph& g, deque<Vertex>& q) {
  graph_traits<Graph>::vertex_iterator i, end;
  for (tie(i, end) = vertices(g); i != end; ++i)
    q.push_front(*i);
}

void link_vertices(Graph& g, deque<Vertex>& list_edges) {
  Edge e;

  for (int i = 0; i < list_edges.size()/2; ++i) {
    cout << "link " << g[list_edges[2*i]].id << " ";
    cout << g[list_edges[2*i+1]].id << endl;
    e = (add_edge(list_edges[2*i], list_edges[2*i+1], g)).first;
  }
}

void nearest_vertex(float& min, Vertex& found, const Vertex& cur, deque<Vertex>& q, const Graph& g) {
  graph_traits<Graph>::adjacency_iterator j, n_end;
  float dist;
  deque<Vertex>::iterator it;

  min = 99999;
  cout << "examining " << g[cur].id << endl;
  cout << "neighbour=";
  for (tie(j, n_end) = adjacent_vertices(cur, g); j != n_end; ++j) {
    // Check if unprocessed
    if (find(q.begin(), q.end(), g[*j].id) != q.end()) {
      cout << g[*j].id << " ";
      dist = custom_distance(g[cur], g[*j]);
      if (dist < min) {
        min = dist;
        found = *j;
      }
    }
  }
  cout << endl;
}

/* Implement nearest neighbour heuristic 
 * The initial graph is modified so we use a copy here */
void nearest_neighbour(Graph& g_final, Graph& g){
  float dist, min;
  int pos, k;
  Vertex cur, u1, u2, found;
  Edge e;
  bool test;
  graph_traits<Graph>::vertex_iterator i, end;
  deque<Vertex>::iterator it;

  deque<Vertex> q;
  init_queue(g, q);

  cur = q.front();
  u1 = add_vertex(g_final);
  g_final[u1] = g[cur];
  q.pop_front();

  while (!q.empty()) {
    //for (int k = 0; k < 12; ++k) {
    cout << "Queue " ;
    for (it = q.begin(); it!=q.end(); ++it) 
      cout << *it << " ";
    cout << endl;

    nearest_vertex(min, found, cur, q, g);

    if (min == 99999) 
      continue;

    // And its clostes neighbour
    u2 = add_vertex(g_final);
    g_final[u2] = g[found];
    cout << "adding " << g_final[u2].id << " ";
    cout << endl;
    //cout << "edge" << g[found].id << "-" << g[cur].id << endl;

    tie(e, test) = add_edge(u1, u2, g_final);//).first;
    g_final[e].length = min;
    cout << "edge " << g_final[u1].id << " " << g_final[u2].id << endl;

    it = find(q.begin(), q.end(), found);
    q.erase(it);

    cur = found;
    u1 = u2;
  }
}

void init_chains(vector<Chain>& list, Graph& g){
  int k = 0;
  graph_traits<Graph>::vertex_iterator i, end;
  list.reserve(num_vertices(g));

  for (tie(i, end) = vertices(g); i != end; ++i) {
    list.push_back(Chain(*i));
  }
  //  for (int k = 0; k < list.size(); ++k) 
  //    cout << g[list[k].end_point(0)].id << " ";
  //    cout << endl;

}

void construct_graph(Graph& g_final, const Graph& g_init, Chain& c) {
  Vertex u, prev;
  Edge e;
  for (int i = 0; i < c.nb_elements(); ++i) {
    //cout << g_init[c.list_element(i)].id << endl;
    u = add_vertex(g_final);
    g_final[u] = g_init[c.list_element(i)];

    if (i > 0) {
      e = (add_edge(u, prev, g_final)).first;
      g_final[e].length = custom_distance(g_final[u], g_final[prev]);
    }
    prev = u;
  }
}

void closest_pair(Graph& g_final, Graph& g){
  float dist, tmp;
  vector<Chain> list;
  Vertex s, t;
  Vertex s_m, t_m;
  int pos1, pos2;
  int head1, head2;

  init_chains(list, g);
  for (int i = 1; i < num_vertices(g); ++i) {
    dist = 9999;
    /* Search over all pair of endpoints */
    for (int j = 0; j < list.size(); ++j) {
      for (int k = 0; k < list.size(); ++k) {
        if (k == j) continue;

        for (int head=0; head < 2; ++head) {
          for (int tail=0; tail < 2; ++tail) {
            s = list[j].end_point(head);
            t = list[k].end_point(tail);

            tmp = custom_distance(g[s], g[t]);
            //cout << "examining " << g[s].id << " " << g[t].id;
            //cout << " dist=" << tmp << endl;
            /* Store the chain and the extremities */
            if (tmp < dist) {
              dist = tmp;
              pos1 = j;
              pos2 = k;
              head1 = head;
              head2 = tail;
            }
          }
        }
      }
    }

    // Give the extremities, not the points to merge
    list[pos1].merge(list[pos2], head1, head2);
    cout << "merging " << pos1 << " " << pos2 << endl;
    cout << "min dist" << dist << endl;
    list[pos1].print(g);
    list.erase(list.begin()+pos2);
  }

  construct_graph(g_final, g, list[0]);
}


void write_dot_graph(const char* filename, const Graph& g, bool edges) {
  // Custom writer
  MyWriter<Graph> w(g);
  ofstream fout(filename);
  if (edges) 
    write_graphviz(fout, g, w,
        make_label_writer(get(&Road::length, g)),
        sample_graph_writer()); 
  else
    write_graphviz(fout, g, w,
        default_writer(),
        sample_graph_writer()); 
}

int main()
{

  srand(10);

  // Graph type, using bundled instead of properties (more readable)
  Graph g_init, g_near, g_close;
  init_graph(g_init);
  write_dot_graph("graph_init.dat", g_init, false);

  nearest_neighbour(g_near, g_init);
  closest_pair(g_close, g_init);

  // Write to file for graphviz
  write_dot_graph("graph_nearest.dat", g_near, true);
  write_dot_graph("graph_closest.dat", g_close, true);
}
