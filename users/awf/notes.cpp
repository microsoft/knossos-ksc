
#include "knossos.h"

double f(Vec x, Mat y) 
{
    return det(y) + norm(x);
}

auto gradf = gradient<f>;

void mymain() {
    Vec x0 = Vec::rand(3);
    Mat y0 = Mat::rand(4,4);

    Vec x_min = minimize(f, x0, y0);
}

double randn();
typedef int PlayerId;

struct Player
{
    double skill;
    double noise;
};

double performance(Player p)
{
    return p.skill + p.noise * randn();
}

bool does_a_win(Player a, Player b)
{
    return performance(a) > performance(b);
}

struct GameOutcome {
    PlayerId a;
    PlayerId b;
    bool a_won;
};

auto infer_skills(list<GameOutcome> data) -> vector<Player>
{
    for(auto game: data) {
        Player& a = players[data.a];
        Player& b = players[data.b];
        observe(does_a_win(a,b) == data.a_won);
    }
}
