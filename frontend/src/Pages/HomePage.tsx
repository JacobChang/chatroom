import React from "react";
import "./HomePage.css";
import { Container } from "../Components/Container";

export class HomePage extends React.Component {
  render() {
    return (
      <div className="page page--home">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h3 className="flex__item">Hurry</h3>
            <button>Join</button>
          </Container>
        </header>
        <section>
          <Container>
            <p>Home Page</p>
          </Container>
        </section>
      </div>
    );
  }
}
