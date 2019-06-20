import React from "react";
import { Container } from "../Components/Container";
import { Link } from "react-router-dom";

export class ChannelPage extends React.Component {
  render() {
    return (
      <div className="page page--channel">
        <header>
          <Container className={["flex__box", "flex__box--vc"]}>
            <h3 className="flex__item">
              <Link to="/">Home</Link>
            </h3>
          </Container>
        </header>
        <section>
          <Container>
            <p>Channel Page</p>
          </Container>
        </section>
      </div>
    );
  }
}
